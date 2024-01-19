---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Bash: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Uso delle Espressioni Regolari in C#

## Cosa & Perché?

Le espressioni regolari, o regex, sono potenti strumenti utilizzati per identificare schemi in stringhe di testo. Consentono di effettuare complesse operazioni di ricerca, verifica e sostituzione su codici con semplicità e precisione.

## Come Fare:

Ecco un esempio di come usare le espressioni regolari in C#. Tutto ciò che devi fare è importare il modulo Regex e utilizzarlo!

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string input = "Ciao, mi chiamo Mario e il mio numero di telefono è 123-456-7890.";
        Regex regex = new Regex(@"\d{3}-\d{3}-\d{4}");
        Match match = regex.Match(input);
        
        if (match.Success)
        {
            Console.WriteLine("Il numero di telefono rilevato è: " + match.Value);
        }
    }
}
```
Questo programma stamperà: `Il numero di telefono rilevato è: 123-456-7890`.

## Approfondimento

Le espressioni regolari sono presenti nella programmazione sin dagli albori. Sono uno strumento ereditato dalla teoria della computazione, diffuso in molti linguaggi di programmazione moderni, incluso C#.

Ci sono alternative alle espressioni regolari per operazioni semplici, come l'uso di metodi stringa predefiniti per la ricerca o la sostituzione. Tuttavia, per operazioni più complesse, le regex spesso offrono la soluzione più flessibile ed efficiente.

In C#, le espressioni regolari sono implementate nel modulo `System.Text.RegularExpressions`, che fornisce metodi per eseguire corrispondenze, ricerche e sostituzioni.

## Per Saperne di Più

Per ulteriori dettagli su come usare le espressioni regolari in C#, visita la [documentazione ufficiale di Microsoft](https://docs.microsoft.com/it-it/dotnet/api/system.text.regularexpressions.regex?view=net-6.0).

Per approfondimenti sulla teoria delle espressioni regolari e su come comporre i tuoi modelli, consulta [Regular-Expressions.info](https://www.regular-expressions.info/tutorial.html).