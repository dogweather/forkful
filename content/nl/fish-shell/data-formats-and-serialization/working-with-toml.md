---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:44.692781-07:00
description: "TOML is een configuratiebestandsformaat, makkelijk voor mensen om te\
  \ lezen en schrijven en eenvoudig voor machines om te parsen en genereren. Programmeurs\u2026"
lastmod: '2024-02-25T18:49:48.595990-07:00'
model: gpt-4-0125-preview
summary: "TOML is een configuratiebestandsformaat, makkelijk voor mensen om te lezen\
  \ en schrijven en eenvoudig voor machines om te parsen en genereren. Programmeurs\u2026"
title: Werken met TOML
---

{{< edit_this_page >}}

## Wat & Waarom?
TOML is een configuratiebestandsformaat, makkelijk voor mensen om te lezen en schrijven en eenvoudig voor machines om te parsen en genereren. Programmeurs werken met TOML voor duidelijke, hiërarchische configuratiebestanden in projecten waar leesbaarheid cruciaal is.

## Hoe te:
Om TOML te lezen en te manipuleren in Fish, kun je een tool zoals `yj` gebruiken, die TOML naar JSON kan converteren. Zo doe je dat:

```fish
# Installeer yj via Fisher
fisher install jorgebucaran/yj

# Converteer TOML naar JSON
echo 'title = "TOML Voorbeeld"' | yj -tj

# Voorbeelduitvoer
{"title":"TOML Voorbeeld"}
```

Om TOML te schrijven, keer je het proces om:

```fish
# Converteer JSON naar TOML
echo '{"title":"JSON Voorbeeld"}' | yj -jt

# Voorbeelduitvoer
title = "JSON Voorbeeld"
```

Voor het zware werk, overweeg een specifieke TOML CLI-tool zoals `toml-cli`.

```fish
# Installeer toml-cli
pip install toml-cli

# Stel een waarde in in TOML-bestand
toml set pyproject.toml tool.poetry.version "1.1.4"

# Haal een waarde uit TOML-bestand
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Diep Duiken
TOML (Tom's Obvious, Minimal Language), geïntroduceerd door Tom Preston-Werner in 2013, lijkt op INI maar met een gedefinieerde spec en gegevenshiërarchie. JSON en YAML zijn de belangrijkste alternatieven, maar ze hebben hun compromissen: JSON is niet zo gebruiksvriendelijk, terwijl YAML complexer is. Het ontwerp van TOML floreert in scenario's waar configuratiebestanden vaak met de hand worden onderhouden, met een balans tussen eenvoud en expressiviteit. Wat implementatie betreft, zijn er TOML-parsers beschikbaar voor de meeste programmeertalen, inclusief TomlBombadil voor Fish dat zo in je scripts kan passen.

## Zie Ook
- Officiële TOML-specificatie: https://toml.io
- `yj`, een tool om te converteren tussen TOML, JSON, YAML, en XML: https://github.com/jorgebucaran/yj
- `toml-cli`, een command-line utility voor TOML: https://github.com/sdispater/toml-cli
