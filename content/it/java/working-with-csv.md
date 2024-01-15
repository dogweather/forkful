---
title:                "Lavorare con i file csv"
html_title:           "Java: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché
Se lavori con dati tabulari, come quelli di un file Excel, è molto probabile che prima o poi ti troverai a doverli gestire in formato CSV. Questo tipo di formato è molto diffuso e spesso utilizzato per lo scambio di dati tra diverse piattaforme. Sapere come lavorare con CSV può semplificare notevolmente il tuo lavoro e rendere la gestione dei dati più efficiente.

## Come 
Ecco un semplice esempio di codice Java che ti mostrerà come leggere e scrivere un file CSV:

```Java
import java.io.FileWriter;
import java.io.IOException;
import java.io.FileReader;
import java.io.BufferedReader;

public class CSVExample {

  public static void main(String[] args) {
  
    // Creiamo un nuovo file CSV
    String csvFilePath = "example.csv";
    
    // Scriviamo alcuni dati nel file
    try {
      FileWriter writer = new FileWriter(csvFilePath);
      writer.append("Nome,Cognome,Età\n"); // Scriviamo l'intestazione del file
      writer.append("Mario,Rossi,30\n"); // Scriviamo la prima riga di dati
      writer.append("Giulia,Bianchi,25\n"); // Scriviamo la seconda riga di dati
      writer.close();
      System.out.println("Il file CSV è stato creato con successo.");
    } catch (IOException e) {
      e.printStackTrace();
    }
    
    // Leggiamo i dati dal file CSV
    try {
      BufferedReader reader = new BufferedReader(new FileReader(csvFilePath));
      String line;
      while ((line = reader.readLine()) != null) { // Leggi finché non arrivi alla fine del file
        String[] data = line.split(","); // Dividi la riga in base al carattere ","
        System.out.println("Nome: " + data[0]); // Stampa il nome
        System.out.println("Cognome: " + data[1]); // Stampa il cognome
        System.out.println("Età: " + data[2]); // Stampa l'età
      }
      reader.close();
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
```

Output:
```
Il file CSV è stato creato con successo.
Nome: Mario
Cognome: Rossi
Età: 30
Nome: Giulia
Cognome: Bianchi
Età: 25
```

## Deep Dive
Oltre alla lettura e scrittura dei dati, esistono numerosi modi per manipolare e analizzare file CSV in Java. Puoi utilizzare librerie come Apache Commons CSV o OpenCSV per semplificare le operazioni di parsing e gestione dei dati. Inoltre, puoi anche utilizzare i metodi della classe String per manipolare le stringhe rappresentanti i dati del CSV.

## Vedi anche
- [Apache Commons CSV](https://commons.apache.org/proper/commons-csv/)
- [OpenCSV](http://opencsv.sourceforge.net/)