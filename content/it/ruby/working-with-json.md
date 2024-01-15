---
title:                "Lavorare con json"
html_title:           "Ruby: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Perché lavorare con JSON?

Se sei interessato ad elaborare dati tra diverse piattaforme o vuoi semplicemente avere un formato di scambio dati più leggibile, allora dovresti considerare di lavorare con JSON. È un formato leggero, facile da leggere e scrivere, e supportato da molti linguaggi di programmazione, inclusa la versione corrente di Ruby.

## Come utilizzare JSON in Ruby

Per prima cosa, dovrai importare la libreria JSON nel tuo file Ruby.

```ruby
require "json"
```

Per convertire un oggetto Ruby in formato JSON, puoi utilizzare il metodo `to_json`.

```ruby
person = { name: "Maria", age: 25, occupation: "Web Developer" }
person_json = person.to_json
```

Per leggere un file JSON e convertirlo in un oggetto Ruby, puoi utilizzare il metodo `JSON.parse`.

```ruby
song_json = File.read("song.json")
song = JSON.parse(song_json)
puts song["title"] # output: "Bohemian Rhapsody"
```

Puoi anche utilizzare la sintassi di `json` per creare un oggetto JSON direttamente.

```ruby
song = {
  title: "Hey Jude",
  artist: "The Beatles",
  genre: "Rock"
}
song_json = JSON.generate(song)
```

## Approfondimento su JSON

- JSON sta per "JavaScript Object Notation" ed è strettamente legato al linguaggio di programmazione JavaScript.
- È formato da coppie chiave-valore e rappresenta dati in formato testo.
- È utilizzato principalmente per il trasferimento di dati tra client e server.

## Vedi anche

- [Documentazione ufficiale di Ruby su JSON](https://ruby-doc.org/stdlib-2.7.1/libdoc/json/rdoc/JSON.html)
- [Tutorial su JSON in Ruby su TutorialsPoint](https://www.tutorialspoint.com/Ruby-JSON)
- [Convertitore JSON online](https://jsonformatter.curiousconcept.com/)