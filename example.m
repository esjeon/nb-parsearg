
<< ParseArg.m

Catch[
  ParseArg`ParseArg[
    $ScriptCommandLine,
    {
      "a" :> Print["Opt 'a'"],
      "b" :> Print["Opt 'b' with string '" <> ReadString[] <> "'"],
      "c" :> Print["Opt 'c' with number '" <> ToString[ReadNumber[]] <> "'"],
      "d" :>
        With[{c=ReadChar[]},
          Which[
            (c == "i"), Print["Opt 'd' with number '" <> ToString[ReadNumber[]] <> "'"],
            (c == "s"), Print["Opt 'd' with string '" <> ReadString[] <> "'"],
            (True), Throw["oops!"]
        ]],
      "default" :> Print["Argument '" <> CurrentToken[] <> "'"],
      _ :> Throw["Invalid flag '" <> CurrentFlag[] <> "'", EParseArg]
    }
  ],
  EParseArg,
  Print["Parse Error: ", #1] &
]

