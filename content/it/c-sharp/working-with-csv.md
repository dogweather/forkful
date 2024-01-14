---
title:                "C#: Lavorare con csv"
simple_title:         "Lavorare con csv"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per cui un programmatore potrebbe dover lavorare con file CSV. Forse si sta scrivendo uno script per l'analisi dei dati, o si sta sviluppando un'applicazione che deve importare e manipolare grandi quantità di informazioni. In ogni caso, imparare a lavorare con CSV può essere incredibilmente utile e risparmiare molto tempo e fatica nel lungo periodo.

## Come Fare

Esistono diverse librerie e metodi per lavorare con file CSV in C#. Oggi, ci concentreremo sull'utilizzo del namespace System.IO per leggere e scrivere file CSV.

Per iniziare, dobbiamo prima includere il namespace "IO" all'inizio del nostro codice:

```C#
using System.IO;
```

Per leggere un file CSV, dobbiamo prima creare un'istanza della classe StreamReader. Passiamo il path del nostro file come argomento al costruttore, come mostrato di seguito:

```C#
StreamReader reader = new StreamReader("mio_file.csv");
```

Ora possiamo utilizzare il metodo "ReadLine" per leggere ogni riga del file come una stringa. Ad esempio, possiamo creare un ciclo while per leggere e stampare ogni riga del nostro file:

```C#
string riga = reader.ReadLine();
while (riga != null)
{
    Console.WriteLine(riga);
    riga = reader.ReadLine();
}
```

Per scrivere un file CSV, useremo invece la classe StreamWriter. Passiamo il path del nostro file e "true" come argomenti al costruttore per specificare che vogliamo aggiungere le nostre righe al file esistente piuttosto che sovrascriverlo:

```C#
StreamWriter writer = new StreamWriter("nuovo_file.csv", true);
```

Possiamo poi utilizzare il metodo "WriteLine" per scrivere ogni riga del nostro file, specificando i valori separati da virgola come argomento:

```C#
writer.WriteLine("Mario, Rossi, 25, Italia");
writer.WriteLine("Maria, Bianchi, 30, Francia");
```

## Approfondimento

Ci sono molte altre funzionalità che è possibile utilizzare quando si lavora con file CSV in C#, ad esempio:

- Utilizzare la classe "TextFieldParser" per analizzare file CSV più complessi che possono contenere valori tra doppi apici o interruzioni di linea.
- Utilizzare il metodo "Split" per dividere una riga in un array di valori separati da virgola.
- Utilizzare il metodo "ToString" per convertire un array di valori in una stringa formattata per essere scritta in un file CSV.

Esplora ulteriormente queste funzionalità e fai pratica con esempi di codice per diventare un esperto nell'utilizzo di CSV in C#.

## Vedi Anche

- [MSDN - Classe StreamReader](https://docs.microsoft.com/it-it/dotnet/api/system.io.streamreader)
- [MSDN - Classe StreamWriter](https://docs.microsoft.com/it-it/dotnet/api/system.io.streamwriter)
- [MSDN - Classe TextFieldParser](https://docs.microsoft.com/it-it/dotnet/api/microsoft.visualbasic.fileio.textfieldparser)
- [MSDN - Metodo Split](https://docs.microsoft.com/it-it/dotnet/api/system.string.split)
- [MSDN - Metodo ToString](https://docs.microsoft.com/it-it/dotnet/api/system.array.tostring)