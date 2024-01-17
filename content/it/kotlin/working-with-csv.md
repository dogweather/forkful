---
title:                "Lavorare con i file csv"
html_title:           "Kotlin: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Lavorare con CSV significa gestire file di testo che contengono dati separati da virgole. I programmatori spesso si trovano ad utilizzare questo formato perché è leggibile, facile da manipolare e compatibile con molti programmi.

## Come fare:
Per gestire un file CSV in Kotlin, è necessario importare la libreria `kotlin-csv` utilizzando Maven o Gradle. Di seguito è riportato un esempio di come leggere un file CSV e ottenere il suo contenuto in una lista di liste di stringhe:

```
Kotlin import com.github.doyaaaaaken.kotlincsv.dsl.csvReader val csvText = File("esempio.csv").readText() val result: List<List<String>> = csvReader{}.readAll(csvText)
```

L'esempio sopra mostra anche come la libreria fornisce uno strumento DSL (Domain Specific Language) per leggere i dati in un formato più intuitivo. Il contenuto del file CSV viene restituito in una lista di liste, in cui ogni sottolista rappresenta una riga del file e ogni elemento è una cella.

Ecco invece un esempio di come scrivere un file CSV a partire da una collezione di oggetti personalizzati:

```
Kotlin import com.github.doyaaaaaken.kotlincsv.dsl.csvWriter data class Person(val name: String, val age: Int) val people = listOf(Person("Giulia", 35), Person("Maurizio", 40), Person("Francesca", 28)) val columns = listOf("Name", "Age") val content = people.map { listOf(it.name, it.age.toString()) } val file: java.io.File = File("persone.csv") csvWriter().open(file) { writeRow(columns) // scrive il nome delle colonne writeAll(content) // scrive il contenuto del file }
```

In questo caso, la libreria offre uno strumento per scrivere i dati in modo ordinato e con la flessibilità di poter specificare il nome delle colonne e il formato delle celle.

## Approfondimento:
Il formato CSV ha origini negli anni '70 e venne introdotto per gestire grandi quantità di dati in modo semplice e leggibile. Oltre al formato CSV standard, esistono anche varianti come TSV (valori separati da tabulazioni) o CSV con delimitatori diversi. I programmatori possono anche utilizzare altri strumenti per manipolare dati tabulari, come ad esempio librerie per il parsing di JSON o XML.

La manipolazione di file CSV può anche essere automatizzata utilizzando script Python o altri linguaggi simili. Tuttavia, con la libreria `kotlin-csv` e l'utilizzo del linguaggio Kotlin, è possibile gestire i dati in modo più robusto e con una sintassi più concisa e moderna.

## Vedi anche:
- Documentazione ufficiale Kotlin per la libreria CSV: https://github.com/doyaaaaaken/kotlin-csv
- Tutorial su come lavorare con file CSV in Kotlin: https://medium.com/@denisbalyko/csv-handling-in-kotlin-ae148bca997e
- Altre librerie utili per manipolare dati tabulari in Kotlin: https://kotlinlang.org/docs/reference/libraries-overview.html#data-processing-and-file-formats