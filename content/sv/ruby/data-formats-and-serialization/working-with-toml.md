---
date: 2024-01-26 04:26:03.030000-07:00
description: "TOML \xE4r ett konfigurationsfilformat som \xE4r l\xE4tt att l\xE4sa\
  \ p\xE5 grund av dess tydliga semantik. Programmerare anv\xE4nder TOML f\xF6r att\
  \ hantera appkonfigurationer\u2026"
lastmod: '2024-03-13T22:44:38.455615-06:00'
model: gpt-4-0125-preview
summary: "TOML \xE4r ett konfigurationsfilformat som \xE4r l\xE4tt att l\xE4sa p\xE5\
  \ grund av dess tydliga semantik."
title: Att arbeta med TOML
weight: 39
---

## Hur man gör:
Först, installera `toml-rb`-smycket. Det är ett populärt val för att tolka TOML i Ruby.

```Ruby
gem install toml-rb
```

Nästa, läsa en TOML-fil:

```Ruby
require 'toml-rb'

toml_innehåll = File.read('config.toml')
konfig = TomlRB.parse(toml_innehåll)
puts konfig['title']
```

Exempelutskrift kan vara:

```
Min Fantastiska App
```

Skriva till en TOML-fil:

```Ruby
require 'toml-rb'

konfig = {
  'title' => 'Min Fantastiska App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_sträng = TomlRB.dump(konfig)
File.write('config.toml', toml_sträng)
```

Kolla `config.toml` och du kommer att se dina inställningar, snyggt lagrade.

## Fördjupning
TOML, som står för Toms Uppebara, Minimala Språk, skapades av Tom Preston-Werner, medgrundare av GitHub, runt 2013. Dess huvudsakliga mål är att vara ett rakt på sak-format som är lätt att tolka till datastrukturer. Medan JSON är bra för API:er och YAML är flexibelt, ligger TOML:s nisch i dess betoning på att vara människovänligt. Till skillnad från YAML, som kan vara kinkigt med indentering, strävar TOML efter en mer INI-lik struktur som många finner enklare och mindre benägen för fel.

Alternativ som JSON, YAML eller XML har alla sina egna styrkor, men TOML frodas i scenarier där en konfig ska vara lätt att underhålla av både människor och program. Det är inte bara enklare utan upprätthåller strikt och läslig formatering.

På den tekniska sidan, för att tolka TOML-innehåll med Ruby, utnyttjar vi smycken som `toml-rb`. Detta smycke drar nytta av Rubys dynamiska natur, konverterar TOML-data till infödda Ruby-hashar, arrayer och andra grundläggande datastrukturer. Denna konvertering innebär att utvecklare kan arbeta med TOML-data med hjälp av bekanta Ruby-semantik och metoder.

## Se även
- TOML-projektet och specifikationen: https://toml.io/en/
- Smycket `toml-rb`: https://github.com/emancu/toml-rb
- Jämförelse mellan TOML, YAML och JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
