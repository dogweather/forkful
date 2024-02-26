---
date: 2024-01-26 04:19:00.947654-07:00
description: "TOML, en kortform f\xF6r Toms Obvious, Minimal Language, \xE4r ett format\
  \ f\xF6r data-serialisering. Programmerare gillar det f\xF6r dess enkelhet och l\xE4\
  sbarhet; det\u2026"
lastmod: '2024-02-25T18:49:36.420765-07:00'
model: gpt-4-0125-preview
summary: "TOML, en kortform f\xF6r Toms Obvious, Minimal Language, \xE4r ett format\
  \ f\xF6r data-serialisering. Programmerare gillar det f\xF6r dess enkelhet och l\xE4\
  sbarhet; det\u2026"
title: Att arbeta med TOML
---

{{< edit_this_page >}}

## Vad & Varför?
TOML, en kortform för Toms Obvious, Minimal Language, är ett format för data-serialisering. Programmerare gillar det för dess enkelhet och läsbarhet; det är toppen för konfigurationsfiler, liknande känsla som YAML men mindre omständligt än JSON för en människa.

## Hur man gör:
Börja med att installera `toml-cli` för att leka med TOML i Bash. Praktiskt för att läsa eller redigera TOML-filer på språng.

```Bash
# Installera toml-cli, vår lilla hjälpreda för TOML-uppgifter
pip install toml-cli

# Tänk dig att du har en TOML-fil, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Läs ett värde
toml get config.toml owner.name
# Output: Tom

# Ange ett värde
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Proffstips: Använd citattecken för nycklar med punkter eller knasiga tecken!
```

## Djupdykning
Född ur ogillandet av JSON:s hinder för människor, dök TOML upp runt 2013. Tom Preston-Werner, medgrundare av GitHub, ville ha något superläsbart. YAML och INI var alternativ, men TOML är som det bästa av båda.

Bom, du har nästlad data och arrayer, minus YAML:s booby traps och JSON:s klammerparenteser. TOML är nu ett förstahandsval för konfig i Rusts Cargo, vilket talar för dess uppgång i utvecklarvärlden. Det drivs av en specifikation, vilket håller saker strama och väldefinierade. Du kommer att kunna hitta parsers på nästan vilket språk som helst, vilket gör det brett antagbart.

## Se även
- Officiella TOML GitHub-repot: https://github.com/toml-lang/toml
- toml-cli på PyPI: https://pypi.org/project/toml-cli/
- Jämförelse av data-serialiseringsformat: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats
