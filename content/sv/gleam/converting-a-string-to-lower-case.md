---
title:                "Konvertera en sträng till små bokstäver"
html_title:           "Gleam: Konvertera en sträng till små bokstäver"
simple_title:         "Konvertera en sträng till små bokstäver"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att konvertera en sträng till små bokstäver är en vanlig operation som programmerare måste utföra när de jobbar med textdata. Det innebär att alla stora bokstäver i en sträng byts ut mot motsvarande små bokstäver. Varför gör man det? För att göra strängar enhetliga och lättare att jämföra och manipulera.

## Så här gör du:

Att konvertera en sträng till små bokstäver är enkelt i Gleam. Använd funktionen `String.to_lower` och skicka in den sträng som du vill konvertera.

```Gleam
let string = "HeLlO wOrLd"
let lower_string = String.to_lower(string)
```

Output: `hello world`

Det är också möjligt att använda `String.to_lower` på en lista av strängar. Då blir varje sträng i listan konverterad.

```Gleam
let strings = ["gLeAm", "iS", "aWeSoMe!"]
let lower_strings = List.map(String.to_lower, strings)
```

Output: `["gleam", "is", "awesome!"]`

## Djupdykning:

Att konvertera strängar till små bokstäver kan spåras tillbaka till de tidiga dagarna av datorer och programmering. Det används ofta för att göra textbearbetning (som sökning och sortering) mer effektivt och enhetligt.

Det finns också andra sätt att konvertera en sträng till små bokstäver, som att använda Unicode-standarden och dess funktioner som `String.to_lowercase`. Gleams funktion `String.to_lower` stöder Unicode, men är också optimerad för att ha hög prestanda och vara enkel att använda i Gleam-kod.

## Se även:

- Gleams officiella dokumentation för `String.to_lower`: [link](https://gleam.run/documentation/stdlib/string.html#to_lower)
- Mer om Unicode och behandling av strängar: [link](https://www.smashingmagazine.com/2019/04/unicode-events-javascript/)