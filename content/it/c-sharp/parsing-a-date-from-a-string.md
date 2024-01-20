---
title:                "Analizzare una data da una stringa"
html_title:           "Fish Shell: Analizzare una data da una stringa"
simple_title:         "Analizzare una data da una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Analisi di una data da una stringa in C#

## Che cosa & Perché?
L'analisi di una data da una stringa consiste nel convertire un testo che rappresenta una data in una variabile di tipo DateTime. I programmatori lo fanno perché le date vengono spesso ricevute come stringhe da file, database o input dell'utente.

## Come si fa:
Ecco un esempio dove si parsa una data da una stringa:
```C#
string dataInStringa = "12/12/2022";
DateTime dataParsed;
if (DateTime.TryParse(dataInStringa, out dataParsed))
{
    Console.WriteLine($"Data parsed correttamente: {dataParsed}");
}
else
{
    Console.WriteLine("Impossibile parsare la data");
}
```
Output:
```
Data parsed correttamente: 12/12/2022 00:00:00
```

## Approfondimento
La classe DateTime in C# fu introdotta nel 2002 quando Microsoft lanciò .NET Framework 1.0. Da allora è diventato uno strumento fondamentale per la manipolazione delle date e delle ore.

Un'alternativa all'uso di DateTime.TryParse è DateTime.Parse, che solleva un'eccezione se la conversione fallisce.
```C#
string dataStringa = "12/12/2222";
DateTime dataParsed = DateTime.Parse(dataStringa);
Console.WriteLine($"Data parsed correttamente: {dataParsed}");
```
Fa attenzione, però, che questo metodo può causare problemi se la stringa non rappresenta una data valida.

Quando si parsa una data da una stringa, .NET tenta di utilizzare il formato di data e la posizione geografica attualmente impostati nel tuo sistema. Se vuoi specificare un formato di data, puoi utilizzare il metodo DateTime.ParseExact o DateTime.TryParseExact.

## Vedi Anche
[Documentazione Microsoft su DateTime.TryParse](https://docs.microsoft.com/it-it/dotnet/api/system.datetime.tryparse?view=net-6.0)
[Documentazione Microsoft su DateTime.Parse](https://docs.microsoft.com/it-it/dotnet/api/system.datetime.parse?view=net-6.0)