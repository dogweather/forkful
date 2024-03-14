---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:07.244802-07:00
description: "TOML is een configuratiebestandsformaat dat gemakkelijk te lezen is\
  \ vanwege de duidelijke semantiek. Programmeurs gebruiken TOML om app-configuraties\
  \ en\u2026"
lastmod: '2024-03-13T22:44:51.393431-06:00'
model: gpt-4-0125-preview
summary: "TOML is een configuratiebestandsformaat dat gemakkelijk te lezen is vanwege\
  \ de duidelijke semantiek. Programmeurs gebruiken TOML om app-configuraties en\u2026"
title: Werken met TOML
---

{{< edit_this_page >}}

## Wat & Waarom?

TOML is een configuratiebestandsformaat dat gemakkelijk te lezen is vanwege de duidelijke semantiek. Programmeurs gebruiken TOML om app-configuraties en data-serialisatie te beheren zonder de zwaarte van XML of de eigenaardigheden van YAML.

## Hoe te:

Installeer eerst de `toml-rb` gem. Dit is een populaire keuze voor TOML-verwerking in Ruby.

```Ruby
gem install toml-rb
```

Vervolgens een TOML-bestand lezen:

```Ruby
require 'toml-rb'

toml_inhoud = File.read('config.toml')
config = TomlRB.parse(toml_inhoud)
puts config['title']
```

Een voorbeelduitvoer zou kunnen zijn:

```
Mijn Geweldige App
```

Schrijven naar een TOML-bestand:

```Ruby
require 'toml-rb'

config = {
  'title' => 'Mijn Geweldige App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

Controleer `config.toml` en je zal je instellingen netjes opgeslagen zien.

## Diepere Duik

TOML, wat staat voor Tom's Obvious, Minimal Language, is gecreëerd door Tom Preston-Werner, de mede-oprichter van GitHub, rond 2013. Het primaire doel is om een overzichtelijk formaat te zijn dat gemakkelijk te ontleden is naar datastructuren. Terwijl JSON geweldig is voor API's, en YAML flexibel is, ligt de niche van TOML in de nadruk op gebruiksvriendelijkheid. Anders dan YAML, dat kieskeurig kan zijn met inspringing, streeft TOML naar een meer INI-achtige structuur die velen eenvoudiger en minder foutgevoelig vinden.

Alternatieven zoals JSON, YAML, of XML hebben elk hun eigen sterke punten, maar TOML bloeit in scenario's waar een configuratie gemakkelijk onderhouden moet kunnen worden door zowel mensen als programma's. Het is niet alleen eenvoudiger maar handhaaft ook strikte en leesbare opmaak.

Aan de technische kant, om TOML-inhoud te ontleden met Ruby, maken we gebruik van gems zoals `toml-rb`. Deze gem maakt gebruik van de dynamische aard van Ruby, door TOML-gegevens om te zetten in native Ruby hashes, arrays en andere basisgegevensstructuren. Deze omzetting betekent dat ontwikkelaars kunnen werken met TOML-gegevens met behulp van bekende Ruby-semantiek en methoden.

## Zie Ook

- TOML project en specificatie: https://toml.io/en/
- De `toml-rb` gem: https://github.com/emancu/toml-rb
- TOML, YAML, en JSON vergelijken: https://blog.theodo.com/2021/08/compare-yml-toml-json/
