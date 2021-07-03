module Malf.Malf

// type MalfAtom = MalfString of string
//               | MalfNumber of float
//               | MalfBool of bool
//               | MalfNil

// type MalfCollection = MalfList
//                     | MalfVector
//                     | MalfHashMap

type MalfReaderMacro = MalfReaderQuote
                     | MalfReaderQuasiquote
                     | MalfReaderUnquote
                     | MalfReaderSpliceUnquote
                     | MalfReaderDeref
                     | MalfReaderMeta

type MalfType = MalfString of string
              | MalfNumber of float
              | MalfBool of bool
              | MalfNil of unit
              | MalfSymbol of string
              | MalfKeyword of string
              | MalfList of MalfType list
              | MalfVector of MalfType array
              | MalfHashMap of Map<string,MalfType>
              | MalfReaderMacro of macroType: MalfReaderMacro * macroInner: MalfType
              | MalfCall of string//(MalfType -> MalfType)
              with
              static member (+) (m0, m1) = 
                  match m0,m1 with
                  | MalfNumber x, MalfNumber y -> MalfNumber (x+y)
                  | _ -> failwith "oops all out of +s"
              static member (-) (m0, m1) = 
                  match m0,m1 with
                  | MalfNumber x, MalfNumber y -> MalfNumber (x-y)
                  | _ -> failwith "oops all out of +s"
              static member (*) (m0, m1) = 
                  match m0,m1 with
                  | MalfNumber x, MalfNumber y -> MalfNumber (x*y)
                  | _ -> failwith "oops all out of +s"
              static member (/) (m0, m1) = 
                  match m0,m1 with
                  | MalfNumber x, MalfNumber y -> MalfNumber (x/y)
                  | _ -> failwith "oops all out of +s"
               
type MalfComment = MalfComment of string //maybe? if i want doctests or something later? 

let addmt (m0:MalfType) (m1:MalfType) : MalfType option =
    match m0,m1 with
    | MalfNumber(x), MalfNumber(y) -> Some(MalfNumber(x+y))
    | _ -> failwith "no inline defined"


[<RequireQualifiedAccess>]
module Reader =
    open FParsec
    type Parser<'t> = Parser<'t, MalfType>
    let test p str =
        printfn "==== fpparsec ===="
        match run p str with
        | Success(result, _, _) -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %A result" errorMsg
        printfn "====/fpparsec ===="
        MalfString str

    let _pfloat = pfloat

    let malfnil = stringReturn "nil" (MalfNil ())
    let malftrue = stringReturn "true" (MalfBool true)
    let malffalse = stringReturn "false" (MalfBool false)
    
    let malfnumber = pfloat |>> MalfNumber

    let str s = pstring s

    let stringLiteral =
        let escape = anyOf "\"\\/bfnrt" <??> "escape"
                     |>> function
                         | 'b' -> "\b"
                         | 'f' -> "\u000C"
                         | 'n' -> "\n"
                         | 'r' -> "\r"
                         | 't' -> "\t"
                         | '\\'-> "\\"
                         | '\"' -> "\""
                         | c -> string c

        let unicodeEscape =
            let hex2int c = (int c &&& 15) + (int c >>> 6)*9

            str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )

        let escapedCharSnippet = str "\\" >>. (escape <|> unicodeEscape)
        let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

        between (str "\"") (str "\"")
                (stringsSepBy normalCharSnippet escapedCharSnippet)

    let malfstring = stringLiteral |>> MalfString

    let malfval, malfvalref = createParserForwardedToRef()

    let ws = opt (many (pchar ',')) >>. spaces

    let malfsym =
        //todo: reorganize this valid symbol list sometime
        let isSymbolFirstCharacter c = isLetter c || c = '_' || c = '+' || c = '*' || c = '-' || c = '>' || c = '/'
        let isSymbolChar c = isLetter c || isDigit c || c = '-' || c = '!' || c = '?' || c = '*' || c = '_' || c = '>' || c = '/'

        many1Satisfy2L isSymbolFirstCharacter isSymbolChar "symbol" |>> MalfSymbol

    let malfcall = malfsym .>>. (ws >>. malfval)
    let validKeywordChars = fun c -> isLetter c || isDigit c
    
    let keywordParser = pipe2 (pchar ':') (manySatisfy (validKeywordChars)) (fun s1 s2 -> string s1 + s2)
    let malfkw = keywordParser |>> MalfKeyword
    //TODO: This needs some work still
    
    let testasdf = MalfList [MalfSymbol "asdf"; MalfNumber 1.0; MalfList [MalfNil(())]]

// MalfList (MalfSymbol MalfNumber MalfList(MalfSym MalfNumber MalfNumber

    let skipcomma = skipString "," |> ignore

    let listBetweenStrings sOpen sClose pElement f =
        between (ws >>. str sOpen) (ws >>. str sClose)
                (ws >>. (sepEndBy (pElement) (ws)) .>> ws |>> f)

    //((str ":" .>>. stringLiteral |>> fun (c,s) -> String.concat "" [c;s]))

    let keyValue = (stringLiteral <|> keywordParser)
                        .>>. (ws >>. malfval)


    let malflist = (listBetweenStrings "(" ")" malfval MalfList) //.>> ws
    let malfvec = (listBetweenStrings "[" "]" malfval (Array.ofList >> MalfVector))
    let malfmap = (listBetweenStrings "{" "}" keyValue (Map.ofList >> MalfHashMap))

    let buildReader readerSym =
        match readerSym with
        | "'" -> pchar (char readerSym) >>. malfval |>> (fun x -> MalfReaderMacro(MalfReaderQuote, x))
        | "`" ->   pchar (char readerSym) >>. malfval |>> (fun x -> MalfReaderMacro(MalfReaderQuasiquote, x))
        | "~" -> pchar (char readerSym) >>. malfval |>> (fun x -> MalfReaderMacro(MalfReaderUnquote, x))
        | "@" -> pchar (char readerSym) >>. malfval |>> (fun x -> MalfReaderMacro(MalfReaderDeref, x))
        | "^" -> pchar (char readerSym) >>. malfmap .>>. (ws >>. malfval) 
                    |>> (fun (m,v) -> MalfReaderMacro(MalfReaderMeta, MalfList([v; m])))
        | "~@" -> pstring readerSym >>. malfval |>> (fun x -> MalfReaderMacro(MalfReaderSpliceUnquote, x))
        | _ -> fail "invalid reader macro created"


    //let malfquote = pchar '\'' >>. malfval |>> (fun x -> MalfReaderMacro(MalfReaderQuote, x))
    
    let malfcomment = pchar ';' >>. restOfLine true |>> ignore //MalfComment 

    let malfreader = choice [buildReader "~@"
                             buildReader "'"
                             buildReader "`"
                             buildReader "~"
                             buildReader "@"
                             buildReader "^"
                             ]

    do malfvalref := choice [malfreader
                             malfstring
                             malfkw
                             malfnumber
                             malftrue
                             malffalse
                             malfnil
                             malfsym
                             malflist
                             malfvec
                             malfmap
                             ]

    let malf = ws >>. malfval .>> ws .>> opt malfcomment .>> ws .>> eof

    let parseMalfParseFailureEOFMessage (str: string) =
        if str.Contains("end of the input stream")
          then "EOF"
          else str

    let parseMalfString str = match run malf str with
                              | Success(result, _, _) -> result
                              | Failure(errMsg, parseError, _) -> MalfString (parseMalfParseFailureEOFMessage errMsg)



let Read'd a = Reader.test Reader.malf a
let Read a = Reader.parseMalfString a

let rec readerMacroTransform a = 
    match a with
    | MalfReaderMacro(mType, mVal) -> 
        match mType with
        | MalfReaderQuote -> MalfList([MalfSymbol("quote"); readerMacroTransform mVal])
        | MalfReaderQuasiquote -> MalfList([MalfSymbol("quasiquote"); readerMacroTransform mVal])
        | MalfReaderUnquote -> MalfList([MalfSymbol("unquote"); readerMacroTransform mVal])
        | MalfReaderSpliceUnquote -> MalfList([MalfSymbol("splice-unquote"); readerMacroTransform mVal])
        | MalfReaderDeref -> MalfList([MalfSymbol("deref"); readerMacroTransform mVal])    
        | MalfReaderMeta -> 
            let (v,m) = match mVal with 
                        | MalfList l -> (l.[0], l.[1])
                        | _ -> failwith "MalfReaderMeta not created with a MalfList inner value"

            MalfList([MalfSymbol("with-meta"); v; m])
    | MalfList l -> List.map (readerMacroTransform) l |> MalfList
    | MalfVector v -> Array.map (readerMacroTransform) v |> MalfVector
    | MalfHashMap kv  -> a
    | _ -> a



let PostReadTransform a = readerMacroTransform a //transform reader macros


module Evaler =
    let env = seq<string * (MalfType -> MalfType -> MalfType) > {
        ("+", (+))
        ("-", (-))
        ("*", (*))
        ("/", (/))
    }
    let environment = Map.ofSeq env
    
    //let foo = environment.["+"] 1 2 
    let foo = (+) 1 2 
    let twoquuz = MalfNumber(2.)
    let quuz = (+) twoquuz twoquuz 
    
    let rec evalAST (ast) (env:Map<string, MalfType -> MalfType -> MalfType>) = 
        printfn "evalAST ast: %A" ast
        match ast with
        | MalfSymbol s -> MalfCall s
        | MalfList l -> 
            List.map eval l |> MalfList
        | _ -> ast

    and eval a = 
        //printfn "eval a: %A" a
        match a with
        | MalfList [] -> a
        | MalfList l ->
                let evaled = evalAST (MalfList l) environment
                printfn "evaled: %A" evaled
                let evalledlist = match evaled with 
                                    | MalfList l -> l
                                    | _ -> []
                let callname = match evalledlist.[0] with MalfCall(s) -> s | _ -> "fail"
                let t = evalledlist.Tail
                
                printf "eval list: s: %A" evaled
                printfn " ||| t: %A" l.Tail

                let callres = match environment.ContainsKey callname with
                              | true -> environment.[callname] (t.[0]) (t.[1])
                              | false -> MalfString "Err"
                
                printfn "eval a result: %A" callres
                
                callres
        | _ -> evalAST a environment



let evalmalf a = Evaler.eval a 

//let _ = printfn "quuz: %A" Evaler.quuz
let Eval a = evalmalf a

module Printer =
    let escapeChars str =
        String.collect (function
                        | '\n' -> "\\\\n"
                        | '"' -> "\\\""
                        | '\\' -> "\\\\"
                        | c -> string c) str
    
    let kwOrStr key =
        let colonp = (string key).IndexOf ":"
        match colonp with
        | 0 -> MalfKeyword key
        | _ -> MalfString key


    let rec printlist l =
        let inner = List.map printmalftype l
        // for foo in inner do printfn "%s" foo
        "(" + (String.concat " " (Seq.ofList inner)) + ")"
    and printvector v =
        let inner = Array.map printmalftype v
        "[" + (String.concat " " (Seq.ofArray inner)) + "]"
    and printmap m =
        let inner = (Map.toSeq m) |> Seq.map (fun (k, v) -> printmalftype (kwOrStr k) + " " + printmalftype v)
        "{" + (String.concat " " inner) + "}"
    and printmalftype m =
        match m with
        | MalfString inner -> "\"" + escapeChars inner + "\""
        | MalfNumber inner -> string inner
        | MalfBool inner -> (string inner).ToLower()
        | MalfNil () -> "nil"
        | MalfSymbol inner -> inner
        | MalfKeyword inner -> inner
        | MalfList inner -> printlist inner
        | MalfVector inner -> printvector inner
        | MalfHashMap inner -> printmap inner
        | MalfReaderMacro (macrotype, inner) -> string (macrotype, inner) //should already be transformed at this point!!(?)
        | MalfCall inner -> "call"
let Print a = Printer.printmalftype a //printmaltype instead??

let Test a =
    // printfn "tet!"
    a

let REP a = a |> Read |> PostReadTransform |> Eval |> Test |> Print
