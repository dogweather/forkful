---
title:                "Lavorare con json"
html_title:           "Gleam: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Lavorare con JSON è una parte importante dello sviluppo di applicazioni web, poiché è il formato più utilizzato per lo scambio di dati tra il server e il client. JSON, acronimo di JavaScript Object Notation, è un formato leggero e semplice da utilizzare per rappresentare dati strutturati, come oggetti e array, utilizzando una sintassi simile a quella di JavaScript. I programmatori utilizzano JSON per gestire dati dinamici, come dati da un database o informazioni provenienti da una richiesta API.

## Come fare:

Utilizzando Gleam, è possibile gestire dati JSON in modo efficiente e intuitivo. Di seguito sono riportati alcuni esempi di codice per comprenderne il funzionamento.

#### Parsing di un file JSON

```Gleam
import gleam/json
import gleam/nodjson

let json = """
{
  "nome": "Mario",
  "cognome": "Rossi",
  "età": 30,
  "interessi": ["musica", "cinema", "sport"]
}
"""

let result = nodjson.parse(json)
```

Il risultato della funzione `parse` è un tipo specializzato di struct Gleam, `DecodeResult`, che contiene i dati json correttamente parsati o un errore.

#### Creazione di un oggetto JSON

```Gleam
import gleam/json
import gleam/write

let mario = json.object([
  "nome" => json.string("Mario"),
  "cognome" => json.string("Rossi"),
  "età" => json.int(30),
  "interessi" => json.array([
    json.string("musica"),
    json.string("cinema"),
    json.string("sport"),
  ])
])

write!(mario)
```

L'output di questo codice è il seguente:

``` 
{
  "nome": "Mario",
  "cognome": "Rossi",
  "età": 30,
  "interessi": ["musica", "cinema", "sport"]
} 
```

## Deep Dive:

JSON è diventato sempre più popolare negli ultimi anni grazie alla sua semplicità ed efficacia, ma non è l'unico formato utilizzato per scambiare dati. Altri formati includono XML e YAML. JSON è stato sviluppato inizialmente per essere utilizzato con JavaScript, ma grazie alla sua sintassi semplice e leggibile, è diventato uno standard per lo scambio di dati anche in altre linguaggi di programmazione.

Gleam utilizza una libreria esterna, `nodjson`, per gestire dati JSON. Questa libreria è basata su [jsone](https://github.com/sile/jsone), scritta in OCaml. Ciò garantisce una rapida e sicura manipolazione dei dati JSON.

## Vedi anche:

- [Gleam repository su GitHub](https://github.com/gleam-lang/gleam)
- [The JSON data format](https://www.json.org/json-it.html)