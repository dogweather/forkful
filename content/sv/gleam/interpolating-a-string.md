---
title:                "Interpolera en sträng"
html_title:           "C++: Interpolera en sträng"
simple_title:         "Interpolera en sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Stränginterpolering innebär att du slår in variabler i en sträng. Varför gör vi det? Enkelt svar: det förenklar sammanfogning av strängar och gör vår kod lättare att läsa.

## Hur gör man:
I Gleam interpolerar vi strängar med hjälp av funktionen `string.append`. Här är exempel:

```gleam
import gleam/string

let namn = "Axel"
let hälsning = string.append(["Hej ", namn, ", hur mår du?"])
```

Koden ovan kommer att returnera: `Hej Axel, hur mår du?`.

## Djupdykning
Stränginterpolering har länge varit en stapelvara i flera programmeringsspråk, inte bara Gleam. Alternativa metoder inkluderar att använda funktioner som `string.concat` eller `+ operator`.

När det gäller genomförandedetaljer i Gleam, översätter `string.append` listelementen till strängar och sammanfogar dem utan ytterligare karaktärer mellan. Detta är snabbare och mer minnesbesparande än att skapa enorma temporära strängar och sedan sammanfoga dem.

## Se också
För mer information om stränginterpolering och relaterade ämnen, se följande källor:

1. Gleam's string documentation: https://hexdocs.pm/docs/gleam/book/String.html
2. Deeper dive into string interpolation in programming: https://realpython.com/python-string-formatting/
3. Further reading on Gleam programming: https://gleam.run/tour/