---
title:                "Java: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui dovresti imparare a lavorare con file CSV in Java. In primo luogo, i CSV (Comma-Separated Values) sono un formato di file molto comune e ampiamente utilizzato nella gestione dei dati. Molte applicazioni e programmi generano dati in formato CSV, quindi sapere come leggerli e scriverli in Java può essere molto utile. Inoltre, i file CSV sono facili da leggere e comprendere, rendendoli perfetti per la condivisione di dati con altre persone o programmi.

## Come fare

Per lavorare con file CSV in Java, la prima cosa da fare è importare la libreria "csv" nel tuo progetto. Puoi farlo aggiungendo la seguente riga di codice all'inizio del tuo file Java:

```Java
import com.opencsv.CSVReader;
```

Una volta importata la libreria, puoi creare un nuovo oggetto CSVReader che utilizzerai per leggere e analizzare il file CSV. Ecco un esempio di come farlo:

```Java
CSVReader reader = new CSVReader(new FileReader("mio_file.csv"));
```

Dopo aver creato il CSVReader, puoi iniziare a leggere i dati dal file. Ad esempio, se il tuo file CSV ha le seguenti colonne: "nome", "cognome" e "età", puoi utilizzare il metodo "readNext()" per ottenere ogni riga del file come array di stringhe, dove ogni elemento corrisponde a una colonna del file. Ecco un esempio di come farlo:

```Java
String[] riga = reader.readNext();
String nome = riga[0];
String cognome = riga[1];
int eta = Integer.parseInt(riga[2]);
```

Dopo aver letto i dati, non dimenticare di chiudere il CSVReader utilizzando il metodo "close()" per liberare le risorse utilizzate.

## Approfondimenti

Ci sono molte altre funzionalità e metodi disponibili nella libreria "csv" per lavorare con file CSV in Java. Ad esempio, puoi specificare un delimitatore diverso dalla virgola, gestire le citazioni nei dati e scrivere dati in un file CSV. Assicurati di consultare la documentazione ufficiale per ulteriori informazioni.

## Vedi anche

- [Documentazione ufficiale della libreria "csv"](http://opencsv.sourceforge.net/)
- [Tutorial su come leggere e scrivere file CSV in Java](https://www.baeldung.com/java-csv-file-array)
- [Esempi pratici di utilizzo della libreria "csv"](https://www.callicoder.com/java-read-write-csv-file-opencsv/)