---
title:                "Lavorare con csv"
html_title:           "C#: Lavorare con csv"
simple_title:         "Lavorare con csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che lavora con dati strutturati, è molto probabile che prima o poi ti troverai a dover utilizzare il formato CSV. Questo è uno dei formati più comuni per la gestione dei dati, quindi è importante sapere come lavorare con esso.

## Come fare

Per iniziare a lavorare con CSV in C#, è necessario includere il namespace `System.IO` per accedere alla classe `StreamReader`. Ci sono diversi modi per leggere un file CSV e convertirlo in una struttura dati più facile da maneggiare. Uno di questi è utilizzando l'oggetto `TextFieldParser` del namespace `Microsoft.VisualBasic.FileIO`.

```C#
using System.IO;
using Microsoft.VisualBasic.FileIO;

// Leggi il file CSV e salva ogni riga in una lista di array di stringhe
static List<string[]> ReadCSV(string filePath)
{
    List<string[]> csvData = new List<string[]>();
    using (TextFieldParser parser = new TextFieldParser(filePath))
    {
        parser.Delimiters = new string[] { "," }; // specifica il separatore dei campi
        while (!parser.EndOfData)
        {
            string[] csvLine = parser.ReadFields(); // leggi la riga e restituisci un array di stringhe
            csvData.Add(csvLine);
        }
    }
    return csvData;
}

// Utilizza la funzione per leggere un file CSV e stamparne il contenuto su console
static void Main()
{
    List<string[]> data = ReadCSV("nome_file.csv");
    foreach (string[] row in data)
    {
        foreach (string cell in row)
        {
            Console.Write(cell + " ");
        }
        Console.WriteLine();
    }
}

//OUTPUT:
//valore1 valore2 valore3
//valore4 valore5 valore6
//...

```

## Approfondimento

Il formato CSV (Comma Separated Values) è un formato di file che viene utilizzato per rappresentare dati in una tabella, dove ogni riga corrisponde a una riga nella tabella e ogni valore è separato da una virgola. CSV è ampiamente utilizzato in applicazioni come fogli di calcolo, database e software di analisi dei dati.

Per lavorare con CSV in modo più avanzato, è possibile utilizzare strumenti come LINQ (Language Integrated Query) per manipolare e filtrare i dati in base alle tue esigenze. Inoltre, è importante prestare attenzione ai caratteri speciali che possono essere presenti nei dati CSV e gestirli correttamente durante il processo di lettura e scrittura.

## Vedi anche

- [C# LINQ - Tutorial per principianti](https://www.c-sharpcorner.com/article/c-sharp-linq-tutorial-for-beginners/)
- [Working with CSV Files in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-read-and-write-to-a-newly-created-data-file)