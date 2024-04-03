---
date: 2024-01-26 04:25:34.436377-07:00
description: "Hur man g\xF6r: Innan vi b\xF6rjar, installera `toml`-paketet med `pip\
  \ install toml`. L\xE5t oss hantera en TOML-fil."
lastmod: '2024-03-13T22:44:37.506771-06:00'
model: gpt-4-0125-preview
summary: "Innan vi b\xF6rjar, installera `toml`-paketet med `pip install toml`."
title: Att arbeta med TOML
weight: 39
---

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
