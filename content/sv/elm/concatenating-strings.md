---
title:                "Sammanslående av strängar"
html_title:           "Elm: Sammanslående av strängar"
simple_title:         "Sammanslående av strängar"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Varför
Att concatenera, eller sammanslå, strängar är en vanlig uppgift inom programmering, oavsett språk. Det är ett bra verktyg för att skapa dynamiskt genererade textsträngar, som används för allt från användargränssnitt till dataformatering.

## Hur man gör
Den grundläggande syntaxen för att concatenera strängar i Elm är att använda operatorn `++`, som står för "concat" eller "sammanslå". Detta kan användas för att sammanfoga två eller flera strängar, eller för att lägga till en sträng till en befintlig sträng. Här är ett enkelt exempel:

```elm
"Hello" ++ "world"    -- Ger "Hello world"
```

Notera att båda operanderna måste vara strängar för att operatorn ska fungera.

Om du vill lägga till flera strängar kan du bara fortsätta att använda `++` operatorn mellan varje sträng. Detta är ett vanligt användningsområde för att bygga upp komplexa textsträngar:

```elm
"Welcome " ++ "to " ++ "my " ++ "website"    -- Ger "Welcome to my website"
```

Ibland kan det vara användbart att inte bara sammanslå strängar, utan också lägga till ett mellanrum mellan dem. Detta kan göras genom att lägga till ett mellanslag som en sträng i sammanslagningen:

```elm
"Hello" ++ " " ++ "world"    -- Ger "Hello world"
```

Slutligen kan vi använda variabler istället för hårkodade strängar i en concatenation. Detta låter oss dynamiskt generera textsträngar baserat på variabler som värden. Här är ett exempel där vi skapar en hälsningsmeddelande baserat på ett användarnamn som sparas i en variabel:

```elm
let
  name = "Anna"
in
  "Hello " ++ name        -- Ger "Hello Anna"
```

## Djupdykning
Förutom operatorn `++` finns det också en inbyggd funktion i Elm som heter `String.concat`, som kan användas för att sammanslå en hel lista av strängar. Istället för att behöva använda `++` flera gånger, kan vi helt enkelt skicka in en lista av strängar till denna funktion och få en sammanslagen sträng tillbaka. Här är ett exempel:

```elm
String.concat ["Hello", " ", "world"]      --Ger "Hello world"
```

Det finns också andra funktioner för att manipulera strängar i Elm, som till exempel `String.reverse` och `String.toUpper`, som kan vara användbara vid projekt som involverar textmanipulation.

## Se också
- ["Strings" in Elm documentation](https://elm-lang.org/docs/syntax#strings)
- ["Strings" in Elm guide](https://guide.elm-lang.org/strings/)