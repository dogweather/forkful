---
title:    "Elm: Omvandla en sträng till gemener"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till gemener är en vanlig uppgift inom programmering. Det är användbart om du till exempel vill jämföra två strängar utan att oroa dig för storleks- eller skiftlägesändringar. Med hjälp av Elm kan du enkelt lösa detta problem på ett effektivt sätt.

## Hur man gör

```Elm
toLowercase : String -> String
toLowercase str =
    String.toLower str
```

I denna enkla kod stycke, använder vi funktionen `toLower` från Elm's `String` modul för att konvertera en sträng till gemener. Vi skickar helt enkelt in vår önskade sträng som argument och funktionen returnerar en ny sträng med alla gemener. Här är ett exempel på hur vår funktion skulle fungera:

```Elm
toLowercase "Elm Programmering"
```

Output:
```Elm
"elm programmering"
```

## Djupdyka

Att konvertera en sträng till gemener kan verka som en enkel uppgift, men det finns faktiskt flera saker att tänka på. Till exempel kan vissa språk ha specialtecken eller bokstäver med accenter som behöver angripas på ett annat sätt för att konvertera dem till gemener. I sådana fall kan det vara användbart att använda sig av en annan funktion från Elm's `String` modul, såsom `toLowerList` som hanterar mer komplexa fall av konvertering.

## Se även

- [Elm's String Modul](https://package.elm-lang.org/packages/elm/core/latest/String)
- [String.toLower dokumentation](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Elm Programmering för nybörjare](https://guide.elm-lang.org/)