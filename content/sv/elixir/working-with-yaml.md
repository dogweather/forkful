---
title:                "Att arbeta med yaml"
html_title:           "Elixir: Att arbeta med yaml"
simple_title:         "Att arbeta med yaml"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

YAML i Elixir

## Vad & Varför?

YAML står för "YAML Ain't Markup Language" och är ett enkelt, mänskligt läsbart dataformat. Det används främst för att konfigurera applikationer och utbyta data mellan olika system. Många programmerare föredrar att använda YAML för sin läsbarhet och flexibilitet.

## Hur man gör:

```Elixir
# Läsa in en YAML-fil som en Elixir-map
YAML.load_file("exempel.yaml")

# Skapa en YAML-fil från en Elixir-map
map = %{namn: "Sara", ålder: 25}
YAML.dump(map)
```

Output: 
```
--- 
namn: Sara
ålder: 25
```

## Djupdykning:

YAML utvecklades år 2001 för att ersätta XML som ett mer human-friendly dataformat. Det är baserat på enkelhet och tydlighet, och kan även användas som ett programmeringsspråk med möjlighet till variabler och loopar. Andra alternativ för konfiguration och datautbyte inkluderar JSON och CSV, men YAML är ofta föredraget för sin läsbarhet och flexibilitet. I Elixir finns en inbyggd modul som erbjuder enkel hantering av YAML-filer, men det finns även tilläggspaket som ger ytterligare funktionalitet.

## Se även:

[Elixir's YAML-modul](https://hexdocs.pm/elixir/YAML.html)