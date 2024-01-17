---
title:                "Interpolazione di una stringa"
html_title:           "C#: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

L'interpolazione di stringhe è un modo efficiente per combinare variabili e testo in una sola stringa. I programmatori lo fanno per rendere il codice più leggibile e facile da mantenere.

## Come fare:

Ecco un esempio di come utilizzare l'interpolazione delle stringhe in C#:

```C#
string nome = "Mario";
int eta = 30;
Console.WriteLine($"Ciao, mi chiamo {nome} e ho {eta} anni.");
```

Questo codice produrrà l'output: "Ciao, mi chiamo Mario e ho 30 anni". Come puoi vedere, l'uso delle parentesi graffe all'interno della stringa indica al programma di sostituire quei valori con il contenuto delle variabili.

## Approfondimento:

L'interpolazione di stringhe è stata introdotta nella versione 6 di C# con l'obiettivo di semplificare la creazione di stringhe composite. In precedenza, i programmatori dovevano utilizzare concatenazioni di stringhe o metodi di formato per raggiungere lo stesso risultato.

Ci sono molti altri modi per combinare variabili e testo in una stringa, come ad esempio l'uso del metodo string.Format() o l'operatore +. Tuttavia, l'interpolazione delle stringhe è spesso considerata la soluzione più chiara e leggibile.

## Vedi anche:

Per ulteriori informazioni su come utilizzare l'interpolazione di stringhe in C#, puoi consultare la documentazione ufficiale di Microsoft su [String Interpolation](https://docs.microsoft.com/it-it/dotnet/csharp/language-reference/tokens/interpolated).