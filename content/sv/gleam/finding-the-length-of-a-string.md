---
title:                "Gleam: Att hitta längden av en sträng"
simple_title:         "Att hitta längden av en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Så varför skulle man vilja engagera sig i att hitta längden på en sträng? Det kan verka som en enkel uppgift, men i verkligheten kan det vara mycket användbart i många programmeringssituationer. Att kunna bestämma längden på en sträng är viktigt för att hantera textdata och för att genomföra olika algoritmer som kräver specificerade teckenmängder.

## Hur man gör det

För att hitta längden på en sträng i Gleam, kan du använda den inbyggda funktionen ```Gleam.length(sträng)```. Det första steget är att skapa en variabel som lagrar strängen du vill undersöka. Sedan kan du använda funktionen ```length``` och ange strängen som argument.

```Gleam
fn hitta_längd(sträng) {
    längd = Gleam.length(sträng)
    IO.print("Längden på " ++ sträng ++ " är " ++ Gleam.toString(length) ++ " tecken.")
}

hitta_längd("Hej världen")
```

Output: "Längden på Hej världen är 11 tecken."

## Djupdykning

Det finns en hel del bakom kulisserna när det gäller att hitta längden på en sträng. I de flesta fall behöver programmerare inte oroa sig för dessa detaljer, men det finns några saker som är värda att nämna.

Först och främst kräver funktionen ```length``` att strängen som matas in är en Unicode-sträng. Om den inte är det, kommer funktionen att returnera en felmeddelande. Detta beror på att Unicode-tecken kan vara mer än 1 byte långa, vilket påverkar längden på en sträng. Detta är speciellt viktigt att tänka på när man arbetar med flerspråkiga eller multibyte-teckenkoder.

Det är också värt att nämna att funktionen inte räknar antalet ord i en sträng, utan bara antalet tecken. Så om du vill räkna antalet ord i en mening, måste du först dela upp strängen med hjälp av mellanslag eller andra tecken och sedan räkna längden på den resulterande lista.

## Se även

- Gleams officiella dokumentation om inbyggda funktioner: https://gleam.run/documentation/stdlib.html#length
- En utförlig förklaring av Unicode och multibyte-teckenkoder: https://www.utf8-chartable.de/
- En tutorial om hur man hanterar textdata i Gleam: https://gleam.run/tutorials/strings.html