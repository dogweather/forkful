---
title:                "Å jobbe med json"
html_title:           "Ruby: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du jobber med data eller utvikler applikasjoner, har du mest sannsynlig støtt på JSON (JavaScript Object Notation). Dette formatet er enkelt å lese og skrive for både mennesker og maskiner, og er derfor et populært valg for å utveksle data mellom ulike systemer og plattformer.

## Hvordan lage og behandle JSON i Ruby

Det første du trenger å gjøre er å inkludere Ruby's JSON-bibliotek med `require "json"`. Deretter kan du lage en Ruby-hasj ved å bruke følgende syntaks:

```Ruby
data = { "navn" => "Lisa", "alder" => 30, "yrke" => "utvikler" }
```

I dette eksempelet har jeg brukt nøkkelordet `=>` for å koble sammen nøklene og verdiene i hasjen. Det er viktig å merke seg at verdiene kan være av forskjellige datatyper, som tekst, tall eller boolske uttrykk.

For å konvertere hasjen til JSON-format kan du bruke `to_json` metoden som følger:

```Ruby
json_data = data.to_json
```

Du kan også lese inn en JSON-fil og konvertere den til en Ruby-hasj ved å bruke `JSON.parse(file)`. Ved å bruke `JSON.pretty_generate` kan du legge til innrykk og linjeskift for å gjøre JSON-en mer lesbar.

```Ruby
file = File.read("data.json")
data = JSON.parse(file)
pretty_json = JSON.pretty_generate(data)
```

Det er også enkelt å skrive ut JSON-data med `puts`, som vil gi følgende output:

```Ruby
{"name":"Lisa","age":30,"occupation":"utvikler"}
```

Hvis du trenger å behandle store mengder med JSON-data, kan du bruke streaming-metoder som `JSON.dump` og `JSON.load` for å unngå å låse opp hele dataen i minnet.

## Dypdykk

Ruby's JSON-bibliotek støtter også ulike alternativer for å tilpasse behandlingen av JSON-data. Du kan for eksempel angi en maksimal dybde på hasjer med `max_nesting`, eller velge å ikke behandle spesielle tegn med `escape` parameteren.

En annen nyttig funksjon er `JSON.generate`, som lar deg angi valgfrie argumenter for å definere et eget formaterings- og indentasjonsnivå for JSON-data.

## Se også

- [Ruby's offisielle JSON-dokumentasjon](https://ruby-doc.org/stdlib-2.6.3/libdoc/json/rdoc/JSON.html)
- [Rubygems-side for JSON-biblioteket](https://rubygems.org/gems/json)