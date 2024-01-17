---
title:                "Lavorare con i file csv"
html_title:           "C#: Lavorare con i file csv"
simple_title:         "Lavorare con i file csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

--------------------------------------

## Cosa & Perché?
Lavorare con CSV (Comma Separated Values) significa manipolare dati in formato tabellare, dove le informazioni sono organizzate in righe e colonne separate da virgole. I programmatori utilizzano spesso il formato CSV perché è molto versatile e compatibile con diversi tipi di software.

## Come fare:
Ecco un esempio di codice che mostra come aprire un file CSV utilizzando C# e stamparne il contenuto:

```C#
using System;
using System.IO; // Importa lo spazio dei nomi per lavorare con i file
using System.Text; // Importa lo spazio dei nomi per lavorare con i caratteri speciali

class Program
{
    static void Main()
    {
        // Percorso del file CSV
        string filePath = @"C:\Users\Utente\Desktop\dipendenti.csv";

        // Usa il metodo StreamReader per leggere il file
        using (StreamReader sr = new StreamReader(filePath, Encoding.UTF8))
        {
            string line;
            while ((line = sr.ReadLine()) != null)
            {
                // Stampa ogni riga del file
                Console.WriteLine(line);
            }
        }
    }
}
```

Ecco un esempio di output ottenuto con questo codice:

```
Nome,Cognome,Età
Mario,Rossi,35
Paolo,Bianchi,40
Luca,Verdi,25
```

## Approfondimento:
Il formato CSV è stato sviluppato negli anni '70 per permettere lo scambio di dati tra diversi software. Anche se originariamente era utilizzato principalmente per fogli di calcolo, ora è ampiamente utilizzato nei database e in altri contesti per la sua semplicità e versatilità.

Esistono anche altri formati di file per rappresentare dati tabellari, come ad esempio il formato Excel (.xlsx), ma spesso il formato CSV è preferito perché è più leggero e facile da lavorare con i dati. Tuttavia, è importante fare attenzione ad eventuali errori di formattazione nel file CSV, poiché possono causare problemi di importazione o esportazione dei dati.

## Vedi anche:
- [Documentazione ufficiale di C# su lavorare con file CSV](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-a-text-file-one-line-at-a-time)
- [Esempi di codice per lavorare con file CSV in C#](https://www.codeproject.com/Tips/387327/Handling-CSVs-using-Csharp)
- [Un'approfondimento sulla storia del formato CSV](https://blog.derefnull.com/convert-csv-to-json/)
- [Un confronto tra diversi formati di file per dati tabellari](https://www.altexsoft.com/blog/datascience/csv-vs-xml-vs-json-vs-yaml-which-is-the-best-for-data-serialization/)