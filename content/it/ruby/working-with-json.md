---
title:                "Ruby: Lavorare con JSON"
simple_title:         "Lavorare con JSON"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Perché

Se sei interessato alla programmazione, probabilmente hai sentito parlare di JSON. È un formato di dati molto popolare e versatile utilizzato per lo scambio di informazioni tra applicazioni. Imparare come lavorare con JSON può aumentare le tue abilità di programmazione e renderlo più facile per te scrivere applicazioni che comunicano tra loro.

## Come Fare

In Ruby, lavorare con JSON è semplice e diretto. Per iniziare, dovrai prima di tutto importare la libreria JSON nel tuo file di codice.

```
require 'json'
```

Una volta importata la libreria, puoi iniziare a convertire i tuoi dati in JSON. Se hai un hash Ruby, puoi utilizzare il metodo `to_json` per convertirlo in una stringa JSON.

```
my_hash = {
  nome: "Mario",
  eta: 30,
  hobby: ["calcio", "pittura"]
}

my_hash.to_json 
```

L'output sarà una stringa JSON formattata correttamente che puoi utilizzare per condividere e scambiare dati tra diverse applicazioni.

```
{"nome":"Mario","eta":30,"hobby":["calcio","pittura"]}
```

Puoi anche utilizzare il metodo `JSON.parse` per convertire una stringa JSON in un hash Ruby.

```
my_string = '{"nome":"Mario","eta":30,"hobby":["calcio","pittura"]}'

JSON.parse(my_string)
```

L'output sarà un hash Ruby simile a quello che abbiamo creato in precedenza.

## Approfondimento

Oltre alla conversione dei dati in JSON, ci sono molte altre cose che puoi fare con questa libreria. Ad esempio, puoi validare una stringa JSON per assicurarti che sia formattata correttamente utilizzando il metodo `JSON.parse` con un blocco di errore.

```
json_string = '{"nome":"Mario","eta":30,"hobby":["calcio","pittura"]]}'

begin
  JSON.parse(json_string)
  puts "Valid!"
rescue JSON::ParserError => e
  puts "Invalid JSON: #{e}"
end
```

Puoi anche utilizzare il metodo `JSON.pretty_generate` per ottenere una stringa JSON ben formattata e leggibile.

```
my_hash = {
  nome: "Mario",
  eta: 30,
  hobby: ["calcio", "pittura"]
}

JSON.pretty_generate(my_hash)

```

L'output sarà:

```
{
  "nome": "Mario",
  "eta": 30,
  "hobby": [
    "calcio",
    "pittura"
  ]
}
```

## Vedi Anche

- [Documentazione ufficiale di Ruby per la libreria JSON](https://ruby-doc.org/stdlib-2.7.1/libdoc/json/rdoc/JSON.html)
- [Un tutorial su come utilizzare JSON in Ruby](https://www.rubyguides.com/2015/01/parsing-json-ruby/)
- [Un articolo su come gestire errori in JSON con Ruby](https://kyan.com/blog/handling-json-parse-errors-in-ruby)