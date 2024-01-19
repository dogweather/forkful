---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Sletting av tegn som stemmer med et mønster er en teknikk som fjerner visse tegn fra en streng. Programmerere bruker dette til å rense data, manipulere tekst og lage spesialiserte algoritmer.

## Sånn Gjør Du:

I Gleam kan du bruke funksjonen `string.replace` for å slette tegn som stemmer med et mønster. Her er et eksempel:

```Gleam
import gleam/string

fn slett_teikn() {
  string.replace("Dette er en setning.", " ", "")
}
slett_teikn()
```
Kjører du denne koden, vil du få følgende resultat:

```Gleam
"Detteerensetning."
```
Som du ser har alle mellomrom blitt fjernet fra setningen.

## Dypdykk

Historisk sett, har sletting av tegn som samsvarer med et mønster blitt brukt i mange programmeringsspråk, ikke bare Gleam. Alternativene til `string.replace` kan være regex-basert søk og erstatning, som tilbyr flere tilpasningsmuligheter, men på bekostning av lesbarhet og ytelse.

Når det gjelder implementeringsdetaljer, `string.replace` i Gleam bruker Erlang's underliggende `re` modul, som er bygget på PCRE biblioteket. Dette gjør `string.replace` veldig effektiv når det samsvarer med store strenger.

## Se Også:

Vurder følgende ressurser for mer informasjon:

1. Gleam String modul dokumentasjon for flere strengmanipulerende funksjoner: https://hexdocs.pm/gleam_stdlib/Gleam.String.html
2. PCRE biblioteket for detaljer om hvordan mønstertjenester fungerer: https://pcre.org/
3. Erlang's `re` modul dokumentasjon for mer informasjon om hvordan `string.replace` er implementert: http://erlang.org/doc/man/re.html