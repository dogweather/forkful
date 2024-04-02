---
date: 2024-01-26 04:21:57.576465-07:00
description: "TOML \xE4r ett config-filformat, enkelt f\xF6r m\xE4nniskor att l\xE4\
  sa och skriva, samt enkelt f\xF6r maskiner att tolka och generera. Programmerare\
  \ arbetar med TOML\u2026"
lastmod: '2024-03-13T22:44:38.361622-06:00'
model: gpt-4-0125-preview
summary: "TOML \xE4r ett config-filformat, enkelt f\xF6r m\xE4nniskor att l\xE4sa\
  \ och skriva, samt enkelt f\xF6r maskiner att tolka och generera. Programmerare\
  \ arbetar med TOML\u2026"
title: Att arbeta med TOML
weight: 39
---

## Vad & Varför?
TOML är ett config-filformat, enkelt för människor att läsa och skriva, samt enkelt för maskiner att tolka och generera. Programmerare arbetar med TOML för tydliga, hierarkiska config-filer i projekt där läsbarheten är avgörande.

## Hur man gör:
För att läsa och manipulera TOML i Fish kan du använda ett verktyg som `yj`, som kan konvertera TOML till JSON. Så här gör du:

```fish
# Installera yj via Fisher
fisher install jorgebucaran/yj

# Konvertera TOML till JSON
echo 'title = "TOML Example"' | yj -tj

# Exempel på utdata
{"title":"TOML Example"}
```

För att skriva TOML gör du processen omvänd:

```fish
# Konvertera JSON till TOML
echo '{"title":"JSON Example"}' | yj -jt

# Exempel på utdata
title = "JSON Example"
```

För tyngre uppgifter, överväg ett dedikerat TOML CLI-verktyg som `toml-cli`.

```fish
# Installera toml-cli
pip install toml-cli

# Ange ett värde i TOML-filen
toml set pyproject.toml tool.poetry.version "1.1.4"

# Hämta ett värde från TOML-filen
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Djupdykning
TOML (Tom's Obvious, Minimal Language), introducerad av Tom Preston-Werner 2013, är lik INI men med en definierad specifikation och datahierarki. JSON och YAML är de främsta alternativen, men de har sina avvägningar: JSON är inte lika användarvänligt, medan YAML är mer komplicerat. TOML:s design utmärker sig i scenarier där config-filer ofta underhålls för hand, och balanserar enkelhet och uttrycksfullhet. När det gäller implementering är TOML-parsare tillgängliga för de flesta programmeringsspråk, inklusive TomlBombadil för Fish som kan sättas direkt in i dina skript.

## Se även
- TOML:s officiella specifikation: https://toml.io
- `yj`, ett verktyg för att konvertera mellan TOML, JSON, YAML och XML: https://github.com/jorgebucaran/yj
- `toml-cli`, ett kommandoradsverktyg för TOML: https://github.com/sdispater/toml-cli
