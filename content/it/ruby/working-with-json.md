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

Ruby: Come lavorare con JSON
JSON (JavaScript Object Notation) è un formato di dati leggibile per le macchine ed è diventato sempre più popolare tra i programmatori per la sua semplicità e flessibilità. È fondamentalmente una collezione di coppie chiave-valore, simile alla struttura degli hash di Ruby, ed è ampiamente utilizzato per condividere dati su Internet.

## Cosa&e perché?
Lavorare con JSON è utile per convertire i dati in un formato facilmente interpretabile dalle macchine, rendendo la comunicazione tra applicazioni e piattaforme più semplice e efficiente. Inoltre, JSON è più leggero rispetto ad altri formati di dati come XML, rendendolo ideale per la trasmissione di grandi quantità di dati.

## Come fare:
Per lavorare con JSON in Ruby, abbiamo bisogno di installare la gemma standard JSON. Questa gemma fornisce metodi d'aiuto utili per la creazione e l'analisi dei file JSON.

```Ruby
# Per prima cosa, installiamo la gemma JSON
gem install json

# Quindi, "richiamiamo" la gemma nel nostro codice Ruby
require 'json'

# Ora possiamo creare un oggetto JSON utilizzando l'hash di Ruby
json_obj = {"nome" => "Mario", "cognome" => "Rossi", "età" => 30}

# Convertiamo l'oggetto JSON in una stringa con il metodo `.to_json`
json_string = json_obj.to_json

# Possiamo anche analizzare una stringa JSON utilizzando il metodo `.parse`
parsed_json = JSON.parse(json_string)
```

## Approfondimento:
JSON è stato originariamente sviluppato da Douglas Crockford nel 2001 ed è diventato uno standard web nel 2013. L'alternativa principale a JSON è XML, tuttavia JSON è diventato più popolare per la sua sintassi più semplice e leggera.

Per implementare il supporto JSON in una web application, si può utilizzare una libreria come [Ruby on Rails](https://rubyonrails.org/) che fornisce funzioni per la serializzazione e deserializzazione dei dati JSON. Inoltre, molti servizi Web esterni, come [Google Maps](https://developers.google.com/maps/documentation/javascript/json) e [Twitter](https://developer.twitter.com/en/docs/twitter-api/v1/tweets/post-and-engage/api-reference/get-statuses-show-id), utilizzano JSON per condividere dati con gli sviluppatori.

## Vedi anche:
* [Documentazione ufficiale di Ruby su JSON](https://ruby-doc.org/stdlib-2.7.2/libdoc/json/rdoc/JSON.html)
* [Sintassi JSON - guida rapida](https://www.youtube.com/watch?v=iiADhChRriM)