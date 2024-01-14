---
title:                "Go: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Molti sviluppatori scelgono di lavorare con file CSV (Comma-Separated Values) per la loro semplicità e compatibilità con molti linguaggi di programmazione. Inoltre, i file CSV sono comunemente utilizzati per il trasferimento e la memorizzazione di dati su diversi sistemi.

## Come Fare

Per lavorare con i file CSV in Go, è necessario importare il pacchetto "encoding/csv". Una volta importato, è possibile utilizzare diverse funzioni per leggere, scrivere e manipolare i dati nei file CSV.

Ecco un esempio di codice per leggere un file CSV e stampare i suoi contenuti:

```
import (
   "encoding/csv"
   "fmt"
   "os"
)

func main() {

   // apre il file CSV
   file, err := os.Open("file.csv")
   if err != nil {
      fmt.Println("Errore durante l'apertura del file")
      return
   }

   // crea un nuovo lettore CSV
   reader := csv.NewReader(file)

   // legge i record dal file
   records, err := reader.ReadAll()
   if err != nil {
      fmt.Println("Errore durante la lettura del file")
      return
   }

   // stampa i dati del file CSV
   for _, record := range records {
      fmt.Println(record)
   }

   file.Close()
}
```

L'output di questo codice sarà una serie di slice, ognuna corrispondente ad una riga del file CSV. È possibile utilizzare gli elementi di queste slice per accedere ai dati specifici di ogni riga.

## Approfondimento

Oltre alla lettura e alla scrittura di dati in file CSV, è possibile utilizzare il pacchetto "encoding/csv" per manipolare i dati in modi più complessi. Ad esempio, è possibile utilizzare la funzione "NewWriter" per creare un nuovo file CSV e scrivere dati all'interno di esso.

Inoltre, è possibile specificare opzioni aggiuntive durante la lettura o la scrittura dei file CSV, come ad esempio il delimitatore dei campi o le citazioni dei valori. Ci sono anche funzioni per convertire i dati in vari tipi di dati di Go, come int o bool.

Per ulteriori informazioni su come lavorare con i file CSV in Go, si consiglia di consultare la documentazione ufficiale del pacchetto "encoding/csv".

## Vedi Anche

- Documentazione ufficiale del pacchetto "encoding/csv": https://golang.org/pkg/encoding/csv
- Tutorial su come lavorare con file CSV in Go: https://www.digitalocean.com/community/tutorials/how-to-read-and-write-csv-files-in-go-it