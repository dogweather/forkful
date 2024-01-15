---
title:                "Lavorare con i file CSV"
html_title:           "Go: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore, probabilmente conosci già il formato CSV (Comma Separated Values). Potrebbe sembrare un formato semplice e banale, ma in realtà è ampiamente utilizzato in molti contesti, come ad esempio per importare ed esportare dati tra diverse applicazioni o per analizzare dati in fogli di calcolo. In questo articolo, scopriremo come lavorare con CSV utilizzando il linguaggio di programmazione Go.

## Come fare

Per iniziare a lavorare con CSV in Go, dovrai installare la libreria "encoding/csv" utilizzando il comando `go get`. Una volta installata, puoi iniziare a utilizzarla nel tuo codice importandola con `import ("encoding/csv")`.

Per leggere un file CSV, utilizza la funzione `csv.NewReader()`, dove dovrai specificare il file e il separatore utilizzato all'interno del file (di solito una virgola o un punto e virgola). Quindi puoi utilizzare il metodo `Read()` per leggere una riga alla volta e `ReadAll()` per leggere tutte le righe del file. Di seguito un esempio:

```Go
// Leggere un file CSV e stampare i dati

package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    // Apriamo il file CSV da leggere
    file, err := os.Open("file.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    // Creiamo il reader CSV
    reader := csv.NewReader(file)

    // Impostiamo il separatore
    reader.Comma = ';'

    // Leggiamo tutte le righe del file e stampiamo i dati
    for {
        // Leggiamo una riga alla volta
        record, err := reader.Read()
        if err != nil {
            // Fine del file
            break
        }

        // Stampa la riga letta
        fmt.Println("Riga letta:", record)
    }
}

```

Utilizzando questo codice, otterremo in output una stampa delle righe lette dal file CSV.

Per scrivere un file CSV, utilizza la funzione `csv.NewWriter()` specificando il file in cui vuoi scrivere e il separatore da utilizzare. Puoi poi utilizzare il metodo `Write()` per scrivere una riga alla volta o `WriteAll()` per scrivere tutte le righe del file. Di seguito un esempio:

```Go
// Scrivere un file CSV

package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    // Creiamo il nostro file CSV
    file, err := os.Create("nuovo_file.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    // Creiamo il writer CSV
    writer := csv.NewWriter(file)

    // Impostiamo il separatore
    writer.Comma = ';'

    // Scriviamo alcune righe nel file
    writer.Write([]string{"Colonna1", "Colonna2"})
    writer.Write([]string{"Dato1", "Dato2"})
    writer.Write([]string{"Altro dato1", "Altro dato2"})

    // Assicuriamoci che tutti i dati siano scritti nel file
    writer.Flush()

    // Stampa un messaggio di successo
    fmt.Println("File CSV scritto con successo!")
}

```

Questo codice scriverà il file CSV con le righe specificate nel codice stesso.

## Approfondimento

Nel codice sopra abbiamo utilizzato il separatore `;` per leggere e scrivere il file CSV. Tuttavia, è possibile impostare il separatore utilizzando anche altri caratteri, come ad esempio una tabulazione o una virgola. Inoltre, la libreria "encoding/csv" ha molte altre funzioni utili che puoi esplorare per adattarle alle tue esigenze.

## Vedi anche

- Documentazione ufficiale sulla libreria "encoding/csv" di Go: https://golang.org/pkg/encoding/csv/
- Esempi di codice su come lavorare con CSV in Go: https://gobyexample.com/reading-files
- Un tutorial su come manipolare i dati CSV utilizzando Go: https://www.callicoder.com/golang-read-write-csv-file/