---
title:    "Gleam: Å bruke regulære uttrykk"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor bruke regulære uttrykk?

Regulære uttrykk er en nyttig verktøy for å håndtere tekst og data på en effektiv måte. De tillater deg å søke, erstatte og manipulere tekstbaserte data ved å bruke forhåndsdefinerte regler. Dette kan være spesielt nyttig når du jobber med store mengder tekst eller utfører repetitivt arbeid.

# Slik bruker du regulære uttrykk i Gleam

For å bruke regulære uttrykk i Gleam, må du først importere modulen "re" og gi den et alias. Deretter kan du bruke funksjonen "match" for å søke etter et gitt mønster i en tekststreng. Her er et eksempel på en enkel søk-operasjon:

```Gleam
let pattern = re.compile("Gleam")
let text = "Jeg elsker å kode i Gleam"
let match = re.match(pattern, text)
```

I dette tilfellet vil variabelen "match" inneholde informasjon om hvor i teksten strengen "Gleam" ble funnet.

# Dypdykk i regulære uttrykk

Regulære uttrykk kan være svært kraftige verktøy når det kommer til tekstmanipulering. Du kan bruke ulike metakarakterer og uttrykk for å finne spesifikke typer tekst, som for eksempel tall, bokstaver eller spesialtegn. Du kan også bruke uttrykk for å søke og erstatte deler av en tekststreng, eller for å splitte en tekststreng inn i ulike deler.

Det er viktig å merke seg at regulære uttrykk kan være komplekse og at det kan ta litt tid å lære seg hvordan de fungerer. Det er derfor alltid lurt å øve seg og eksperimentere for å bli mer komfortabel med dem.

# Se også
- [Gleam offisiell dokumentasjon for regulære uttrykk](https://gleam.run/lib/re.html)
- [Primer på regulære uttrykk i Gleam](https://gleam.run/articles/regex-primer.html)
- [Interaktive øvelser for å lære regulære uttrykk](https://regexone.com)