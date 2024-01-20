---
title:                "Arbeide med yaml"
html_title:           "Elm: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
YAML er et format for å strukturere og organisere data, spesielt brukt i programmering og konfigurasjonsfiler. Det er en mer leselig og intuitiv måte å representere data på, sammenlignet med mer komplekse formater som JSON og XML. Mange programmører velger å bruke YAML på grunn av dens enkelhet og lesbarhet.

## Hvordan:
For å jobbe med YAML i Elm, trenger du en pakke kalt `elm-yaml`. Med denne kan du enkelt konvertere YAML-filer til Elm-typer og omvendt. Her er et enkelt eksempel på hvordan du kan bruke `elm-yaml`:

```Elm
import Yaml exposing (..)

-- Definer YAML-data
yamlData = "age: 28\nname: John"

-- Konverter YAML til Elm-typer
elmData = decode yamlData

```

I dette eksempelet vil `elmData` bli en Elm-tuple med `("age", 28)` og `("name", "John")` som elementer. For å konvertere fra Elm til YAML, kan du bruke funksjonen `encode`:

```Elm
-- Konverter Elm-typer til YAML
yaml = encode elmData

```

Denne funksjonen vil produsere en streng med YAML-formatert data, klar til å bli lagret i din konfigurasjonsfil.

## Dykk dypere:
YAML ble utviklet i 2001 av Clark Evans for å være en mer menneskelig-leselig og enklere alternativ til JSON og XML. Det har blitt populært i utviklingen av kuber, Docker og andre infrastrukturverktøy på grunn av dets evne til å representere komplekse datastrukturer på en intuitiv måte. Alternativer til YAML inkluderer toml og HCL. Implementasjonen av `elm-yaml` er basert på libyaml og bruker PEG for parsing av YAML.

## Se også:
- [Offisiell YAML-nettside](https://yaml.org/)