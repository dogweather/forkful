---
title:                "Omvandla en sträng till gemener"
html_title:           "Gleam: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför
Det är vanligt att behöva hantera textsträngar i programmering, och ibland kan det vara nödvändigt att konvertera dem till en viss form. Att konvertera en sträng till små bokstäver är en vanlig operation som kan behövas av olika skäl, till exempel för att underlätta sökning eller för att jämföra strängar utan att bry sig om deras stor- eller små bokstäver.

## Såhär gör du
För att konvertera en sträng till små bokstäver i Gleam kan du använda funktionen `String.to_lower` som tar emot en sträng som argument och returnerar en ny sträng med alla bokstäver i små bokstäver.

```Gleam
my_string = "HeLlo WoRlD"
lower_case_string = my_string |> String.to_lower
```

Output: `"hello world"`

Om du vill kan du också använda funktionen `String.to_lower_case_all` som har samma funktion, men tar emot en lista av strängar och returnerar en lista av konverterade strängar.

```Gleam
my_strings = ["HeLlo", "WoRlD"]
lower_case_strings = my_strings |> String.to_lower_case_all
```

Output: `["hello", "world"]`

## Djupdykning
Det finns flera anledningar till att man vill konvertera en sträng till små bokstäver, till exempel för jämförelse eller för att underlätta sökning. Det kan också vara användbart för att få en enhetlig form på strängar som innehåller blandade stor- och små bokstäver.

Det finns även andra metoder för att konvertera strängar till olika former, som till exempel `String.to_upper`, `String.to_title` och `String.to_snake_case`. Dessa funktioner kan vara användbara beroende på vilken form du vill ha på din sträng.

## Se även
- [Gleam dokumentation om strängar](https://gleam.run/core/string.html)
- [Gleam String modul på GitHub](https://github.com/gleam-lang/gleam_stdlib/blob/main/std/string.gleam)