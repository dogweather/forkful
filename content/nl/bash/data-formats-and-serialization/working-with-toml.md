---
title:                "Werken met TOML"
aliases:
- /nl/bash/working-with-toml.md
date:                  2024-01-28T22:10:35.502313-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/bash/working-with-toml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
TOML, afkorting voor Tom's Obvious, Minimal Language, is een data-serialisatieformaat. Programmeurs zijn er dol op vanwege de eenvoud en leesbaarheid; het is uitstekend voor configuratiebestanden, vergelijkbare vibes met YAML maar minder omslachtig dan JSON voor een mens.

## Hoe te:
Allereerst, installeer `toml-cli` om met TOML in Bash te spelen. Handig voor het lezen of bewerken van TOML-bestanden ter plekke.

```Bash
# Installeer toml-cli, onze kleine helper voor TOML taken
pip install toml-cli

# Stel je hebt een TOML-bestand, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Lees een waarde
toml get config.toml owner.name
# Uitvoer: Tom

# Stel een waarde in
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Pro tip: Gebruik aanhalingstekens voor sleutels met punten of gekke tekens!
```

## Diepe Duik
Geboren uit de aversie tegen de belemmeringen van JSON voor mensen, verscheen TOML rond 2013. Tom Preston-Werner, mede-oprichter van GitHub, wilde iets super leesbaars. YAML en INI waren alternatieven maar TOML is als het beste van beide.

Shebang, je hebt geneste gegevens en arrays, min de voetgeweren van YAML en de krullende haakjes van JSON. TOML is nu een go-to voor configuratie in Rust's Cargo, wat spreekt voor zijn opkomst in de ontwikkelwereld. Het wordt aangedreven door een specificatie, waardoor alles strak en goed gedefinieerd blijft. Je zult parsers in bijna elke taal aantreffen, waardoor het breed adopteerbaar is.

## Zie Ook
- Officiële TOML GitHub Repo: https://github.com/toml-lang/toml
- toml-cli op PyPI: https://pypi.org/project/toml-cli/
- Vergelijking van data-serialisatieformaten: https://nl.wikipedia.org/wiki/Vergelijking_van_data-serialisatieformaten
