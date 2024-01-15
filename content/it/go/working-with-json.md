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

## Perché

Se sei un programmatore o stai cercando di diventarlo, è probabile che prima o poi dovrari lavorare con JSON. JSON (JavaScript Object Notation) è un formato di dati molto comune utilizzato per lo scambio di informazioni tra diversi sistemi. Con Go, hai a disposizione strumenti potenti per gestire i dati JSON in modo efficiente e facile.

## Come fare

Per lavorare con JSON in Go, devi utilizzare il package "encoding/json". Puoi utilizzare la funzione "Marshal" per convertire una struttura Go in un formato JSON e la funzione "Unmarshal" per convertire una stringa JSON in una struttura Go. Ad esempio, se vuoi creare un oggetto JSON per rappresentare una persona con nome, cognome e età, puoi farlo come segue:

```Go
// Definizione della struttura della persona
type Persona struct {
    Nome       string
    Cognome    string
    Età        int
}
// Creazione di una nuova persona
nuovaPersona := Persona{Nome: "Marco", Cognome: "Rossi", Età: 35}
// Conversione della struttura in formato JSON
datiJSON, _ := json.Marshal(nuovaPersona)
// Stampa del risultato
fmt.Println(string(datiJSON))
```

L'output sarà:

```Go
{"Nome":"Marco","Cognome":"Rossi","Età":35}
```

Per convertire una stringa JSON in una struttura Go, puoi utilizzare la seguente sintassi:

```Go
// Definizione della struttura della persona
type Persona struct {
    Nome       string
    Cognome    string
    Età        int
}
// Definizione della stringa JSON
datiJSON := `{"Nome":"Giulia","Cognome":"Bianchi","Età":27}`
// Creazione di una nuova struttura che rappresenta la persona
var persona Persona
// Conversione della stringa JSON nella struttura Go
err := json.Unmarshal([]byte(datiJSON), &persona)
// Controllo degli eventuali errori
if err != nil {
    fmt.Println(err)
}
// Stampa del risultato
fmt.Println(persona)
```

L'output sarà:

```Go
{Giulia Bianchi 27}
```

## Approfondimento

Mentre i due esempi precedenti mostrano come convertire una struttura in formato JSON e viceversa, ci sono alcune cose importanti da tenere presente quando si lavora con JSON in Go. In particolare, è fondamentale tenere conto della corretta gestione degli errori durante la conversione dei dati. Inoltre, è importante sapere come gestire i tipi di dati vuoti o null all'interno di un oggetto JSON. Puoi trovare ulteriori informazioni in merito in documenti come "Effective Go" e la documentazione ufficiale di Go sul package "encoding/json".

## Vedi anche

- [Documentazione ufficiale di Go sul package "encoding/json"](https://golang.org/pkg/encoding/json/)
- [Documentazione di "Effective Go" sul package "encoding/json"](https://golang.org/doc/effective_go.html#json)
- [Tutorial su come utilizzare il package "encoding/json" in Go](https://www.sohamkamani.com/blog/golang/2018-06-20-golang-using-json-marshalling/)