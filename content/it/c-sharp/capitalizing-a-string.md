---
title:                "Maiuscolizzare una stringa"
html_title:           "C#: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizzare una Stringa in C#: Tutto quello che Devi Sapere

## Cos'è & Perché?

Capitalizzare una stringa significa convertire la prima lettera di ogni parola in maiuscolo. I programmatori lo fanno per migliorare la visualizzazione dei dati e l'usabilità, una pratica comune nelle interfacce utente.

## Come fare:

È semplice capitalizzare una stringa in C#. Qui c'è un esempio:

```C#
string frase = "ciao mondo";
string capitalizzata = System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase(frase.ToLower());
Console.WriteLine(capitalizzata);
```

Il risultato del codice sopra sarà:

```C#
"Ciao Mondo"
```

## Approfondimento

La funzione `ToTitleCase()` è presente nel Framework .NET da quando è stata introdotta la sua prima versione nel 2002. È implementata all'interno della classe `TextInfo` in `System.Globalization`. 

Esistono alternative a `ToTitleCase()`, per esempio usando LINQ o espressioni regolari, ma spesso queste offrono soluzioni più complesse e meno efficienti.

Un dettaglio importante riguardo `ToTitleCase()` è che opera solo sulle lettere che seguono un carattere non alfabetico. Quindi, per assicurarci che funzioni correttamente, convertiamo prima la stringa in minuscolo usando `ToLower()`.

## Vedi anche

- Documentazione Microsoft sulla classe TextInfo: [qui](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo?view=net-5.0)
- StackOverflow con discussioni e alternative sulla capitalizzazione delle stringhe: [qui](https://stackoverflow.com/questions/4480134/string-capitalize)