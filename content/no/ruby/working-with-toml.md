---
title:                "Jobbe med TOML"
date:                  2024-01-26T04:25:49.606796-07:00
model:                 gpt-4-0125-preview
simple_title:         "Jobbe med TOML"

category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/ruby/working-with-toml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

TOML er et konfigurasjonsfilformat som er lett å lese på grunn av sine klare semantikker. Programmerere bruker TOML for å håndtere app-konfigurasjoner og dataseriering uten bryet med XML eller særegenheter ved YAML.

## Hvordan:

Først, installer `toml-rb`-gemmen. Det er et populært valg for TOML-tolkning i Ruby.

```Ruby
gem install toml-rb
```

Deretter, lesing av en TOML-fil:

```Ruby
require 'toml-rb'

toml_innhold = File.read('config.toml')
konfig = TomlRB.parse(toml_innhold)
puts konfig['title']
```

Eksempel på utskrift kan være:

```
Min Fantastiske App
```

Skriving til en TOML-fil:

```Ruby
require 'toml-rb'

konfig = {
  'title' => 'Min Fantastiske App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_streng = TomlRB.dump(konfig)
File.write('config.toml', toml_streng)
```

Sjekk `config.toml`, og du vil se innstillingene dine, pent lagret.

## Dypdykk

TOML, som står for Toms Opplagte, Minimale Språk, ble skapt av Tom Preston-Werner, medgrunnleggeren av GitHub, rundt 2013. Hovedmålet er å være et greit format som er lett å tolke til datastrukturer. Mens JSON er flott for APIer, og YAML er fleksibelt, er TOMLs nisje vektleggingen på å være brukervennlig. I motsetning til YAML, som kan være nøye med innrykk, sikter TOML mot en mer INI-lignende struktur som mange finner enklere og mindre feilutsatt.

Alternativer som JSON, YAML eller XML har hver sine styrker, men TOML trives i scenarier der en konfig bør være lett å vedlikeholde av både mennesker og programmer. Det er ikke bare enklere, men pålegger strenge og lesbare formateringer.

På den tekniske siden, for å tolke TOML-innhold med Ruby, bruker vi gems som `toml-rb`. Denne gemmen utnytter Rubys dynamiske natur, ved å konvertere TOML-data til innfødte Ruby-hashmaps, arrays og andre grunnleggende datastrukturer. Denne konverteringen betyr at utviklere kan arbeide med TOML-data ved å bruke kjente Ruby-semantikker og metoder.

## Se Også

- TOML-prosjekt og spesifikasjon: https://toml.io/en/
- `toml-rb`-gemmen: https://github.com/emancu/toml-rb
- Sammenligning av TOML, YAML og JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
