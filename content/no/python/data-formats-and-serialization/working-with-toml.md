---
title:                "Jobbe med TOML"
aliases: - /no/python/working-with-toml.md
date:                  2024-01-26T04:25:30.285670-07:00
model:                 gpt-4-0125-preview
simple_title:         "Jobbe med TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/python/working-with-toml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
TOML, som er kort for Toms Opplagte, Minimale Språk, er et data serialiseringsformat i slekt med JSON eller YAML, men har som mål å være enkelt og leselig. Programmerere bruker TOML for konfigurasjonsfiler fordi det er enkelt å skrive og forstå, og det kartlegger pent på datastrukturer i programmeringsspråk som Python.

## Hvordan:
Før du dykker inn, installer `toml`-pakken med `pip install toml`. La oss parse en TOML-fil:

```python
import toml

# Eksempel på TOML-innhold som en streng
toml_streng = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # Førsteklasses datoer

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Parse TOML-strengen
parsed_toml = toml.loads(toml_streng)

# Tilgang til data
print(parsed_toml['owner']['name'])  # Utdata: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # Utdata: [8001, 8001, 8002]
```

## Dypdykk
TOML ble skapt av Tom Preston-Werner, en av grunnleggerne av GitHub, som et mer brukervennlig konfigurasjonsfilformat. Det er designet for utvetydig å kartlegge til en hashtabell og være enkelt parsbart av maskiner.

Sammenlignet med JSON, er TOML mer leselig for konfigurasjonsfiler og støtter kommentarer. YAML, et annet alternativ, kan være mer kompakt, men dets avhengighet av innrykk og subtile problemer, som hvordan tabber ikke er tillatt, kan forvirre folk.

Når det gjelder implementeringsdetaljer, er TOML-verdier typet, som inkluderer strenger, heltall, flyttall, boolske verdier, dato/tid, matriser og tabeller. Alt er skriftsensitivt. Også, TOML støtter flerlinjestrenger og, per den siste versjonen, tillater til og med heterogent typede matriser.

Python bruker `toml`-biblioteket, som speiler JSON og YAML-bibliotekene når det gjelder API. Du har `toml.load` og `toml.loads` for å lese TOML fra en fil eller en streng, henholdsvis, og `toml.dump` og `toml.dumps` for å skrive det ut.

## Se Også
- Det offisielle TOML GitHub-repositoriet for spesifikasjoner: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Dokumentasjonen for `toml` Python-biblioteket: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Virkelige eksempler på TOML: Konfigurasjonsfiler for Rusts pakkebehandler `cargo` eller Python-pakkeringsverktøyet `poetry`.
