---
aliases:
- /it/ruby/working-with-toml/
date: 2024-01-26 04:25:45.924006-07:00
description: "TOML \xE8 un formato di file di configurazione che \xE8 facile da leggere\
  \ grazie alla sua chiara semantica. I programmatori usano TOML per gestire le\u2026"
lastmod: 2024-02-18 23:08:56.405522
model: gpt-4-0125-preview
summary: "TOML \xE8 un formato di file di configurazione che \xE8 facile da leggere\
  \ grazie alla sua chiara semantica. I programmatori usano TOML per gestire le\u2026"
title: Lavorare con TOML
---

{{< edit_this_page >}}

## Cosa & Perché?

TOML è un formato di file di configurazione che è facile da leggere grazie alla sua chiara semantica. I programmatori usano TOML per gestire le configurazioni delle app e la serializzazione dei dati senza il peso di XML o le peculiarità di YAML.

## Come fare:

Per prima cosa, installa la gemma `toml-rb`. È una scelta popolare per l'analisi di TOML in Ruby.

```Ruby
gem install toml-rb
```

Poi, per leggere un file TOML:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

Un esempio di output potrebbe essere:

```
My Awesome App
```

Per scrivere in un file TOML:

```Ruby
require 'toml-rb'

config = {
  'title' => 'My Awesome App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

Controlla `config.toml` e vedrai le tue impostazioni, ordinatamente memorizzate.

## Approfondimento

TOML, che sta per Tom's Obvious, Minimal Language, è stato creato da Tom Preston-Werner, il co-fondatore di GitHub, intorno al 2013. Il suo obiettivo principale è essere un formato semplice che sia facile da analizzare in strutture dati. Mentre JSON è ottimo per le API e YAML è flessibile, la nicchia di TOML è la sua enfasi sull'essere amichevole per l'essere umano. A differenza di YAML, che può essere pignolo con l'indentazione, TOML mira a una struttura più simile a INI che molti trovano più semplice e meno soggetta a errori.

Alternative come JSON, YAML o XML hanno ognuno i propri punti di forza, ma TOML si distingue negli scenari in cui una configurazione dovrebbe essere facilmente mantenuta sia dagli umani che dai programmi. Non è solo più semplice, ma impone una formattazione rigorosa e leggibile.

Sul lato tecnico, per analizzare il contenuto TOML con Ruby, ci affidiamo a gemme come `toml-rb`. Questa gemma sfrutta la natura dinamica di Ruby, convertendo i dati TOML in hash, array e altre strutture dati di base native di Ruby. Questa conversione significa che gli sviluppatori possono lavorare con i dati TOML utilizzando la semantica e i metodi familiari di Ruby.

## Vedi anche

- Progetto e specifica di TOML: https://toml.io/en/
- La gemma `toml-rb`: https://github.com/emancu/toml-rb
- Confronto tra TOML, YAML e JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
