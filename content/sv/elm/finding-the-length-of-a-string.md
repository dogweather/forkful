---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att hitta längden på en sträng innebär att räkna antalet tecken i den. Programmerare gör detta för att kontrollera dataingångar eller bearbeta text i program.

## Så här gör du:
I Elm, används `String.length` för att hitta längden på en sträng. Här är ett exempel:

```Elm
import String

lengthOfString : String -> Int
lengthOfString str = String.length str

main = 
  lengthOfString "Hej Sverige" 
  |> toString
  |> text
```

När du kör detta program, kommer utmatningen att vara `11` vilket är antalet tecken i strängen "Hej Sverige".

## Djupdykning
Emellertid, `String.length` i Elm räknar kodpunkter, inte tecken i en sträng. Till exempel, "ö" räknas som en kodpunkt, inte två. Men i andra programmeringsspråk eller tidigare versioner av Elm, kan "ö" räknas som två tecken.

För de flesta syften, behöver du inte tänka på denna skillnad. Men om du behandlar internationella strängar (som innehåller emoji eller icke-latinska tecken) behöver du vara medveten om detta.

Alternativt kan du använda `List.length << String.toList` för att räkna tecken i stället för kodpunkter. Men det kan vara långsammare än `String.length`.

```Elm
lengthOfString : String -> Int
lengthOfString str = List.length (String.toList str)
```

## Se Också
Ellie, Elm’s online editor, har bra exempel på `String.length` och är en bra plats att öva:
https://ellie-app.com/cZv8kGR6V5ba1

Elm’s officiella dokumentation om `String.length`:
https://package.elm-lang.org/packages/elm/core/latest/String#length

Ett exempel på GitHub om hur du använder `String.length` i Elm:
https://github.com/rofrol/elm-string-length-example