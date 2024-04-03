---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:43.003009-07:00
description: "Il formato Valori Separati da Virgola (CSV) \xE8 ubiquitario per lo\
  \ scambio di dati a causa della sua semplicit\xE0 e facilit\xE0 di integrazione\
  \ con la maggior\u2026"
lastmod: '2024-03-13T22:44:42.932426-06:00'
model: gpt-4-0125-preview
summary: "Il formato Valori Separati da Virgola (CSV) \xE8 ubiquitario per lo scambio\
  \ di dati a causa della sua semplicit\xE0 e facilit\xE0 di integrazione con la maggior\
  \ parte dei linguaggi di programmazione, inclusi Go."
title: Lavorare con CSV
weight: 37
---

## Come fare:
Lavorare con file CSV in Go è semplice, grazie alla sua libreria standard, `encoding/csv`. Di seguito è presentato un primo approccio alla lettura e scrittura di file CSV.

### Leggere un File CSV
Per leggere da un file CSV, si apre prima il file usando `os.Open`, quindi si crea un nuovo lettore CSV con `csv.NewReader`.

```go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("data.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    records, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    for _, record := range records {
        fmt.Println(record)
    }
}
```

Questo frammento di codice leggerà tutti i record da `data.csv` e li stamperà. Ogni record è una slice di campi.

### Scrivere in un File CSV
Per scrivere, si usa `csv.NewWriter` e `writer.WriteAll` o `writer.Write` per scrivere rispettivamente più record CSV o un singolo record CSV.

```go
package main

import (
    "encoding/csv"
    "os"
)

func main() {
    file, err := os.Create("output.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    writer := csv.NewWriter(file)
    defer writer.Flush()

    records := [][]string{
        {"Nome", "Età", "Città"},
        {"John Doe", "30", "New York"},
        {"Jane Doe", "27", "Los Angeles"},
    }

    if err := writer.WriteAll(records); err != nil {
        panic(err)
    }
}
```

Ciò creerà un file chiamato `output.csv` con i record forniti. Ricordarsi sempre di svuotare il buffer del writer per assicurarsi che tutti i dati in buffer vengano scritti nel file.

## Approfondimento
Il pacchetto Go `encoding/csv` fornisce un supporto robusto per la lettura e la scrittura di file CSV, ma è progettato con la semplicità in mente, il che significa che non gestisce scenari più complessi come l'auto-rilevamento dei delimitatori, il trattamento delle virgolette o degli a capo incorporati nei campi senza una gestione manuale.

Storicamente, la gestione dei CSV nei linguaggi di programmazione è spesso stata ingombrante a causa di queste complessità, ma la libreria standard di Go astrae molte di queste questioni, consentendo agli sviluppatori di lavorare con dati CSV con relativa facilità. Tuttavia, per manipolazioni CSV più complesse, potrebbe essere necessario utilizzare librerie di terze parti come `gocsv` o gestire il parsing manualmente.

Un aspetto notevole del pacchetto `csv` di Go è il suo supporto per la specificazione di virgole personalizzate (delimitatori), che gli consente di funzionare senza problemi con varianti di file CSV, come i valori separati da tabulazioni (TSV). Tuttavia, quando si lavora con file CSV altamente irregolari o non standard, i programmatori Go potrebbero trovarsi nella necessità di estendere le implementazioni esistenti del lettore o scrittore csv.

Sebbene le capacità di gestione dei CSV in Go siano robuste per scopi generali, per applicazioni che richiedono una manipolazione intensiva dei dati, come la data science o compiti di trasformazione dati complessi, i programmatori potrebbero guardare a pacchetti di elaborazione dati dedicati o persino ad altri linguaggi più adatti a queste attività, come Python con la sua libreria `pandas`. Tuttavia, per operazioni di lettura-scrittura CSV dirette, la libreria standard di Go si distingue per la sua efficienza e semplicità.
