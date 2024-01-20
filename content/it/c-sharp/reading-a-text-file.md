---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lettura di un file di testo in C# 

## 1. Che cosa & Perché?
La lettura di un file di testo è un'operazione che permette al tuo codice di importare informazioni da un file esterno nel tuo programma. Questo è spesso fatto per motivi di efficienza, modularità del codice, o per processare grandi quantità di dati.

## 2. Come fare:
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string testo = File.ReadAllText(@"C:\Esempio.txt");
        Console.WriteLine(testo);
    }
}
```
Questo frammento di codice legge un file di testo dal percorso specificato e stampa il contenuto nella console.

Input file (Esempio.txt):  
```
Ciao, mondo!
```
Output:  
```
Ciao, mondo!
```
## 3. Approfondimento
Nel contesto storico, la lettura di file era onnipresente sin dalle prime fasi dell'elaborazione dei dati. Una pratica antico ma ancora largamente in uso.

Come alternativa, potresti adoperare diversi metodi di lettura come File.ReadAllLines o StreamReader, a seconda delle tue necessità. File.ReadAllText legge tutto il contenuto del file in una volta, mentre StreamReader lo fa in maniera più controllata.

Riguardo ai dettagli di implementazione, `File.ReadAllText` in C# legge un file e restituisce il suo contenuto come una stringa. Questo metodo è più efficiente nel caso di file di piccole dimensioni, ma presta attenzione quando si tratta di file troppo grandi, dato che potrebbe provocare un'insufficienza di memoria.

## 4. Guarda anche
Per ulteriori informazioni, visita i seguenti link:

1. Documentazione ufficiale Microsoft: [File.ReadAllText Method](https://docs.microsoft.com/it-it/dotnet/api/system.io.file.readalltext?view=netframework-4.8)

2. StackOverflow: [How to read a large text file in C#](https://stackoverflow.com/questions/2161895/reading-large-text-files-with-streams-in-c-sharp)

3. Dot Net Perls: [File.ReadAllText](https://www.dotnetperls.com/file-readalltext)