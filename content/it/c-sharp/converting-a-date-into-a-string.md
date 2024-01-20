---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Da Data a Stringa in C#: Una Guida Passo-Passo

## Cosa e Perché?

La conversione da data a stringa trasforma un oggetto DateTime in una stringa leggibile. Questo rende la visualizzazione e la manipolazione dei dati più intuitive per gli utenti.

## Come si fa:

Convertiamo una data in una stringa usando il metodo `ToString()`, così:

```C#
DateTime dataOra = DateTime.Now;
string dataOraStringa = dataOra.ToString();
Console.WriteLine(dataOraStringa);
```

Ecco un esempio di output:

```C#
"02/12/2022 14:25:38"
```

È possibile personalizzare il formato della stringa specificando un formato di stringa nel metodo `ToString()`:

```C#
DateTime dataOra = DateTime.Now;
string dataOraStringa = dataOra.ToString("dd/MM/yyyy");
Console.WriteLine(dataOraStringa);
```

Ecco un esempio di output personalizzato:

```C#
"02/12/2022"
```

## Approfondimento

Questa tecnica si basa sulla classe `DateTime` introdotta in C# 2002. Non c'è una specifica alternativa a `DateTime.ToString()`, ma possiamo personalizzare ulteriormente il formato su misura. Ad esempio, possiamo includere il nome del giorno utilizzando il formato "dddd":

```C#
DateTime dataOra = DateTime.Now;
string dataOraStringa = dataOra.ToString("dddd, dd MMMM yyyy");
Console.WriteLine(dataOraStringa);
```

Output:

```C#
"venerdì, 02 Dicembre 2022"
```

Sfruttare la personalizzazione del formato può rendere i tuoi programmi più intuitivi e facili da usare per i tuoi utenti.

##Riferimenti Utili 

- [Documentazione Microsoft su DateTime](https://docs.microsoft.com/it-it/dotnet/api/system.datetime?view=net-6.0)
- [Guida di personalizzazione della stringa di formato di data e ora](https://docs.microsoft.com/it-it/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Stack Overflow: Converting DateTime to String in C#](https://stackoverflow.com/questions/1245825/converting-datetime-to-string-in-c-sharp)