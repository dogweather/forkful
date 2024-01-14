---
title:                "Kotlin: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Lavorare con file CSV è molto comune nella programmazione e può essere utile in una varietà di situazioni. Potresti dover manipolare grandi quantità di dati in un formato tabellare, oppure potresti essere coinvolto in un progetto che utilizza un database relazionale. In entrambi i casi, lavorare con CSV può semplificare il processo di importazione e export dei dati.

## Come fare

Per lavorare con file CSV in Kotlin, è possibile utilizzare la libreria "kotlin-csv". Iniziamo importando la libreria nel nostro progetto:

```Kotlin
import com.github.doyaaaaaken.kotlincsv.dsl.csvWriter
import com.github.doyaaaaaken.kotlincsv.dsl.csvReader
```

Successivamente, possiamo creare un nuovo file CSV con valori di esempio:

```Kotlin
val newCSV = arrayOf(
    listOf("Nome", "Cognome", "Età"),
    listOf("Mario", "Rossi", "35"),
    listOf("Laura", "Bianchi", "28")
)
```

Per scrivere il nostro nuovo file CSV, possiamo utilizzare il seguente codice:

```Kotlin
csvWriter().open("nuovo_file.csv", append = true) {
    writeAll(newCSV)
}
```

In questo modo, il nuovo file CSV con i nostri valori di esempio verrà creato nella stessa cartella del codice. Per leggere un file CSV esistente, possiamo utilizzare il seguente codice:

```Kotlin
val csvData = csvReader().readAll("esempio.csv")
```

Infine, possiamo iterare sui dati e stamparli in console:

```Kotlin
csvData.forEach { row ->
    println("${row[0]} ${row[1]} ha ${row[2]} anni")
}
```

L'output di questo codice sarà:

```
Mario Rossi ha 35 anni
Laura Bianchi ha 28 anni
```

## Approfondimento

Per lavorare con file CSV in Kotlin, è possibile utilizzare sia la libreria "kotlin-csv" che la classe "CSVFormat" delle librerie standard di Java. Entrambe offrono una varietà di funzioni e metodi che consentono di manipolare i dati CSV in modo efficiente.

Inoltre, è importante prestare attenzione alla formattazione del file CSV. Ci sono alcune convenzioni da seguire, come l'utilizzo di virgole come delimitatori e il corretto trattamento delle righe e delle colonne che contenono spazi o caratteri speciali. 

Per una guida più approfondita su come lavorare con file CSV in Kotlin, consiglio di consultare la documentazione ufficiale della libreria "kotlin-csv" e di esplorare le varie funzionalità offerte da "CSVFormat".

## Vedi anche

- [Documentazione ufficiale della libreria kotlin-csv](https://github.com/doyaaaaaken/kotlin-csv)
- [Guida per la manipolazione di file CSV in Kotlin](https://www.baeldung.com/kotlin/csv)
- [Documentazione ufficiale di CSVFormat dalle librerie standard di Java](https://docs.oracle.com/javase/8/docs/api/java/io/CSVFormat.html)