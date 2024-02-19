---
aliases:
- /sv/python/working-with-toml/
date: 2024-01-26 04:25:34.436377-07:00
description: "TOML, en f\xF6rkortning f\xF6r Toms Obvious, Minimal Language, \xE4\
  r ett format f\xF6r dataseriering liknande JSON eller YAML, men som siktar p\xE5\
  \ enkelhet och\u2026"
lastmod: 2024-02-18 23:08:51.444218
model: gpt-4-0125-preview
summary: "TOML, en f\xF6rkortning f\xF6r Toms Obvious, Minimal Language, \xE4r ett\
  \ format f\xF6r dataseriering liknande JSON eller YAML, men som siktar p\xE5 enkelhet\
  \ och\u2026"
title: Att arbeta med TOML
---

{{< edit_this_page >}}

## Vad och varför?
TOML, en förkortning för Toms Obvious, Minimal Language, är ett format för dataseriering liknande JSON eller YAML, men som siktar på enkelhet och läsbarhet. Programmerare använder TOML för konfigurationsfiler eftersom det är lätt att skriva och förstå, och det kartlägger snyggt på datastrukturer i programmeringsspråk som Python.

## Hur man gör:
Innan vi börjar, installera `toml`-paketet med `pip install toml`. Låt oss hantera en TOML-fil:

```python
import toml

# Exempel på TOML-innehåll som en sträng
toml_strang = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # Datum av första klass

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Tolka TOML-strängen
parsed_toml = toml.loads(toml_strang)

# Åtkomst av data
print(parsed_toml['owner']['name'])  # Output: Tom Preston-Werner
print(parsed_toml['database']['ports'])  # Output: [8001, 8001, 8002]
```

## Djupdykning
TOML skapades av Tom Preston-Werner, en av grundarna till GitHub, som ett mer användarvänligt konfigurationsfilformat. Det är utformat för att entydigt kartlägga till en hashtabell och vara lätt tolkbara av maskiner.

Jämfört med JSON är TOML mer läsbart för konfigurationsfiler och stöder kommentarer. YAML, ett annat alternativ, kan vara mer kompakt, men dess beroende av indrag och subtila problem, som hur tabbar inte är tillåtna, kan ställa till det för människor.

När det kommer till implementeringsdetaljer, har TOML typade värden, vilket inkluderar strängar, heltal, flyttal, booleska värden, datumtid, arrayer och tabeller. Allt är skiftlägeskänsligt. Dessutom stöder TOML flerradiga strängar och, i den senaste versionen, tillåter även heterogent typade arrayer.

Python använder `toml`-biblioteket, som speglar JSON- och YAML-biblioteken i termer av API. Du har `toml.load` och `toml.loads` för att läsa TOML från en fil eller en sträng, respektive, och `toml.dump` och `toml.dumps` för att skriva ut det.

## Se även
- Det officiella TOML GitHub-repositoriet för specifikationer: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Dokumentationen för `toml`-biblioteket i Python: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Verkliga exempel på TOML: Konfigurationsfiler för Rust's pakethanterare `cargo` eller Python-emballageverktyget `poetry`.
