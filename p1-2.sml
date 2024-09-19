(*
Name(s):  Ellie Alberty and Erik Claros
Email(s): eja028@latech.edu
Date: 2/24/24
Course Number and Section: CSC 330 001
Quarter: Spring 2024
Project #: p1
*)

(*instantiates the datatype token that defines the allowed tokens when parsing*)
datatype token = INT | MAI | RET | OP | CP | OB | CB | SC | IN of int;

(*Helper function for firstWord that iterates through the list created by firstWord to find the index of the 
first true item in the list. This allows the function firstWord to locate which item in inputs the string is 
matching with*)
fun findTrue [] = ~1
    | findTrue (true::xs) = 0
    | findTrue (false::xs) = 1 + findTrue(xs);

(*This function finds the first word that matches with one of the allowed words in inputs ussing the isPrefix 
function from the built in String library to return which word is matched and denote invalid text as X*)
fun firstWord text = 
    let 
        val inputs = ["int", "main", "return", "(", ")", "{", "}", ";", " ", "\n", "X"]
    in
        List.nth(inputs, findTrue(map (fn target:string => String.isPrefix target text) inputs))
    end;

(*this is a simple helper function for makeWordList that converts an int option to int*)
fun fromOption NONE = ~1
    | fromOption (SOME x) = x;

(*This is the most involved function that mimics the tokens function in the string library but does not require
a space between tokens in the string. It checks if the next word could be an int using the from string function 
and if it is (returning SOME i) it converts it to string and appends recurs. Else, it calls the firstWord function
to find the first word and then recurs the rest of the list*)
fun makeWordList "" = []
  | makeWordList text =
      case Int.fromString(text) of
        NONE =>
          let
            val first = firstWord(text)
            val rest = implode(List.drop(explode(text), size(first)))
          in
            first :: makeWordList(rest)
          end
  | SOME i =>
          let
            val numberStr = Int.toString(i)
            val rest = implode(List.drop(explode(text), size(numberStr) + 1))
          in
            numberStr :: makeWordList(rest)
          end;


fun translate "int" = SOME INT
  | translate "main" = SOME MAI
  | translate "return" = SOME RET
  | translate "(" = SOME OP
  | translate ")" = SOME CP
  | translate "{" = SOME OB
  | translate "}" = SOME CB
  | translate ";" = SOME SC
  | translate " " = NONE
  | translate "\n" = NONE
  | translate x = 
    case Int.fromString x of
      SOME x => SOME (IN x)
    | NONE => raise Fail "Invalid Parser";

fun helperTranslate [] = []
  | helperTranslate (x::xs) = (translate x)::(helperTranslate xs);

(*This converts the option list to a regular list after the translate function is run to get rid of all of the 
NONEs (aka whitespace)*)
fun fromOptionList [] = []
    | fromOptionList (NONE::xs) = fromOptionList(xs)
    | fromOptionList (SOME x::xs) = x::fromOptionList(xs);

(*
fun parse file = 
  let 
    val file = TextIO.openIn file 
    val text = TextIO.inputAll file
    val _ = TextIO.closeIn file
    val words = filterWhiteSpace(makeWordList(text))
  in
    helperTranslate(words)
  end;
*)

fun parse file = fromOptionList(helperTranslate(makeWordList(TextIO.input(TextIO.openIn(file)))));

parse("CSC330/p1test.c");

