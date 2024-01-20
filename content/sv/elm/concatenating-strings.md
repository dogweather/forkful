---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konkatenera strängar betyder att sammansätta två eller flera strängar i ett gemensamt uttryck. Programmerare gör detta för att bygga sammanhängande meddelanden, användargränssnitt och för att bearbeta datavärden som text.

## Hur man gör:
I Elm, vi använder `++` operatorer för att konkatenera strängar. Låt oss visa det med några kodexempel.

```Elm
"hej" ++ " " ++ "värld" -- Resultatet blir "hej värld"
```
Eller, man kan konkatenera strängar och tulpaner med `++` och `toString`:

```Elm
"Året är " ++ toString 2022 -- Resultatet blir "Året är 2022"
```

## Djupgående
Historiskt sett, konkatenering av strängar representerade en grundläggande operation i tidiga programmeringsspråk som Fortran och COBOL. I Elm, det visas sig genom att använda `++` operator. Det finns alternativ till `++`, som `String.concat`, som tar en lista av strängar och kombinerar dem till en enda sträng. Strängkonkatenering i Elm är strikt modifiering av strängar. Eftersom Elm är ett funktionellt språk, redan existerande strängar är aldrig ändrat, istället nya strängar är skapade när strängar konkateneras.

## Se även
Läs mer om strängar på Elm's officiella dokumentation: [The String module in Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
Liknande begrepp och tillvägagångssätt i andra funktionella programmeringsspråk: [Concatenation in Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:-43--43-) och  [Concatenation in F#](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/symbol-and-operator-reference/)