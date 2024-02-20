---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:36.672150-07:00
description: "TOML, een afkorting voor Tom's Obvious, Minimal Language, is een data-serialisatieformaat\
  \ vergelijkbaar met JSON of YAML, maar streeft naar eenvoud en\u2026"
lastmod: 2024-02-19 22:05:09.488295
model: gpt-4-0125-preview
summary: "TOML, een afkorting voor Tom's Obvious, Minimal Language, is een data-serialisatieformaat\
  \ vergelijkbaar met JSON of YAML, maar streeft naar eenvoud en\u2026"
title: Werken met TOML
---

{{< edit_this_page >}}

## Wat & Waarom?
TOML, een afkorting voor Tom's Obvious, Minimal Language, is een data-serialisatieformaat vergelijkbaar met JSON of YAML, maar streeft naar eenvoud en leesbaarheid. Programmeurs gebruiken TOML voor configuratiebestanden omdat het makkelijk te schrijven en begrijpen is, en het netjes in kaart brengt op datastructuren in programmeertalen zoals Python.

## Hoe doe je het:
Voordat je begint, installeer het `toml` pakket met `pip install toml`. Laten we een TOML-bestand parsen:

```python
import toml

# Voorbeeld TOML-inhoud als een string
toml_string = """
[eigenaar]
naam = "Tom Preston-Werner"
geboortedatum = 1979-05-27T07:32:00Z # Eersteklas data

[database]
server = "192.168.1.1"
poorten = [ 8001, 8001, 8002 ]
"""

# Parse de TOML string
geparste_toml = toml.loads(toml_string)

# Toegang tot gegevens
print(geparste_toml['eigenaar']['naam'])  # Output: Tom Preston-Werner
print(geparste_toml['database']['poorten'])  # Output: [8001, 8001, 8002]
```

## Diepduik
TOML is gecreëerd door Tom Preston-Werner, een van de oprichters van GitHub, als een gebruiksvriendelijker configuratiebestandsformaat. Het is ontworpen om ondubbelzinnig in kaart te brengen op een hashtabel en gemakkelijk door machines te parseren.

Vergeleken met JSON is TOML leesbaarder voor configuratiebestanden en ondersteunt het opmerkingen. YAML, een ander alternatief, kan compacter zijn, maar zijn afhankelijkheid van inspringing en subtiele problemen, zoals dat tabbladen niet zijn toegestaan, kan mensen in verwarring brengen.

Wat implementatiedetails betreft, zijn TOML-waarden getypeerd, wat strings, integers, floats, booleans, datums, arrays en tabellen omvat. Alles is hoofdlettergevoelig. Ook ondersteunt TOML meerregelige strings en, vanaf de laatste versie, zelfs heterogeen getypeerde arrays.

Python gebruikt de `toml` bibliotheek, die qua API overeenkomt met de JSON- en YAML-bibliotheken. Je hebt `toml.load` en `toml.loads` voor het lezen van TOML uit een bestand of een string, respectievelijk, en `toml.dump` en `toml.dumps` voor het uitschrijven ervan.

## Zie Ook
- De officiële TOML GitHub-repository voor specificaties: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- De `toml` Python-bibliotheek documentatie: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Praktijkvoorbeelden van TOML: Configuratiebestanden voor Rust's pakketbeheerder `cargo` of de Python verpakkingshulpmiddel `poetry`.
