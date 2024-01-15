---
title:                "Hitta längden på en sträng"
html_title:           "Gleam: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att hitta längden på en sträng är en grundläggande uppgift för alla programmerare, oavsett vilket språk de använder. Att kunna bestämma längden på en sträng är viktigt för att kunna manipulera och bearbeta data på ett effektivt sätt.

## Hur

För att hitta längden på en sträng i Gleam behöver du bara använda dig av den inbyggda funktionen `String.length()`. Här är ett exempel på hur du kan använda denna funktion:

```Gleam
let str = "Hej, världen!"
let length = String.length(str)
```

I det här exemplet skapas en variabel `str` som innehåller strängen "Hej, världen!" Sedan kallas `String.length()` funktionen med `str` som input, och resultatet tilldelas till en variabel `length`. Om vi nu skriver ut värdet på `length` kommer vi att få resultatet 13, eftersom detta är längden på vår sträng.

## Djupdykning

I bakgrunden är en sträng i Gleam egentligen bara en lista av tecken som representerar varje enskilt tecken i strängen. Därför kan vi också hitta längden på en sträng genom att använda oss av listfunktionen `List.length()`:

```Gleam
let str = "Hej, världen!"
let length = str
  |> String.to_list
  |> List.length
```

I detta exempel konverteras strängen först till en lista av tecken med hjälp av `String.to_list` funktionen. Sedan används `List.length` för att räkna antalet tecken i listan, vilket ger oss längden på strängen.

## Se även

För mer information om strängar och andra datatyper i Gleam, se följande länkar:

- [Officiell Gleam dokumentation](https://gleam.run/documentation/index.html)
- [Gleam exempel](https://github.com/gleam-lang/gleam/tree/master/examples)
- [Gleam community forum](https://forum.gleam.run/)