---
title:                "Att arbeta med TOML"
date:                  2024-01-26T04:26:03.030000-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med TOML"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/working-with-toml.md"
---

{{< edit_this_page >}}

## Vad & Varför?

TOML är ett konfigurationsfilformat som är lätt att läsa på grund av dess tydliga semantik. Programmerare använder TOML för att hantera appkonfigurationer och dataserialisering utan tyngden av XML eller egenskaperna hos YAML.

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
