---
title:                "Lavorare con csv"
html_title:           "Kotlin: Lavorare con csv"
simple_title:         "Lavorare con csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando con dati tabulari, come ad esempio informazioni finanziarie o dettagli su prodotti, è molto probabile che il formato CSV (Comma Separated Values) sia uno dei formati di file più comunemente utilizzati per l'importazione e l'esportazione dei dati. Conoscere come lavorare con CSV è quindi un'abilità importante per chiunque si trovi a gestire informazioni tabulari.

## Come fare

Lavorare con CSV in Kotlin è molto semplice grazie al suo sistema di input/output integrato. Ecco un semplice esempio di codice per leggere un file CSV e stamparne il contenuto:

```Kotlin
val file = File("dati.csv")
file.forEachLine {
    val riga = it.split(",")
    println("Nome: ${riga[0]}, Cognome: ${riga[1]}, Età: ${riga[2]}")
}
```
Esempio di output:
```
Nome: Marco, Cognome: Rossi, Età: 33
Nome: Laura, Cognome: Bianchi, Età: 28
Nome: Luca, Cognome: Verdi, Età: 42
```

Per scrivere su un file CSV, si può utilizzare il seguente codice:

```Kotlin
val dati = listOf(
    listOf("Paolo", "Rossi", "29"),
    listOf("Chiara", "Bianchi", "25"),
    listOf("Andrea", "Verdi", "35")
)
val file = File("dati.csv")
dati.forEach {
    file.appendText("${it[0]}, ${it[1]}, ${it[2]}")
}
```

Esempio di output (contenuto del file dati.csv):
```
Paolo, Rossi, 29
Chiara, Bianchi, 25
Andrea, Verdi, 35
```

## Approfondimento

Per lavorare con CSV in modo più avanzato, è possibile utilizzare la libreria Apache Commons CSV. Questa libreria fornisce diverse funzionalità utili per la gestione di file CSV, come la possibilità di leggere e scrivere su file con diversi formati dei delimitatori (ad esempio, virgola, tab) e la gestione di valori contenenti caratteri speciali.

Inoltre, se stai lavorando con grandi quantità di dati, potrebbe essere utile utilizzare la libreria Kotlinx-Serialization per serializzare e deserializzare oggetti a/dal formato CSV. Ciò può semplificare notevolmente la gestione dei dati e rendere il tuo codice più efficiente.

## Vedi anche

- Documentazione Kotlin sul sistema di input/output: https://kotlinlang.org/docs/tutorials/kotlin-for-py/little-toy-backend.html#support
- Documentazione Apache Commons CSV: https://commons.apache.org/proper/commons-csv/
- Documentazione Kotlinx-Serialization: https://github.com/Kotlin/kotlinx.serialization