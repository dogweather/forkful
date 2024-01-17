---
title:                "Lavorare con json"
html_title:           "Go: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-json.md"
---

{{< edit_this_page >}}

"## Che cosa & Perché?"

Lavorare con JSON significa interagire con dati formattati in modo leggibile da una macchina attraverso lo scambio di messaggi. I programmatori spesso si trovano ad utilizzare JSON perché è uno standard di fatto per lo scambio di dati tra server e applicazioni web.

"## Come fare:"

Per lavorare con JSON in Go, è necessario utilizzare il pacchetto standard "encoding/json". Di seguito un semplice esempio di codice che mostra come creare una struttura di dati JSON, serializzarla e quindi deserializzarla in un oggetto Go:

```Go
package main

import (
    "encoding/json"
    "fmt"
)

type Persona struct {
    Nome string `json:"nome"` // specifica il nome del campo in JSON
    Cognome string `json:"cognome"`
    Età int `json:"età"`
}

func main() {
    p := Persona{Nome: "Mario", Cognome: "Rossi", Età: 35}

    // Serializza la struttura in formato JSON
    b, _ := json.Marshal(p)
    fmt.Println(string(b)) // output: {"nome": "Mario", "cognome": "Rossi", "età": 35}
    
    // Deserializza il JSON in un oggetto Go
    var p2 Persona
    json.Unmarshal(b, &p2)
    fmt.Println(p2.Nome) // output: Mario
}
```

"## Approfondimento:"

JSON, acronimo di JavaScript Object Notation, è un formato di dati basato su JavaScript ma indipendente dal linguaggio, il che lo rende accessibile a diversi linguaggi di programmazione. È nato come alternativa più leggera e facile da usare rispetto al formato XML, ed è diventato uno standard molto popolare nel mondo della programmazione.

Come alternativa al pacchetto standard "encoding/json", esistono anche altre librerie come "jsoniter" o "easyjson" che offrono prestazioni migliori o funzionalità aggiuntive.

La struttura dei dati JSON è composta da coppie di nome e valore, dove il valore può essere un oggetto, un array o un valore singolo come una stringa, un numero o un Booleano. È anche possibile definire specifici tag per i campi in modo da personalizzare la serializzazione e deserializzazione tramite il pacchetto "encoding/json".

"## Vedi anche:"

- Documentazione ufficiale di Go su "encoding/json": https://golang.org/pkg/encoding/json/
- Libreria "jsoniter" per una maggiore efficienza nella manipolazione di dati JSON in Go: https://github.com/json-iterator/go
- Libreria "easyjson" per una maggiore flessibilità nel generare codice Go dalle definizioni JSON: https://github.com/mailru/easyjson