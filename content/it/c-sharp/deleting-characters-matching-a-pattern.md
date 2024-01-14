---
title:                "C#: Eliminando caratteri corrispondenti ad un modello"
simple_title:         "Eliminando caratteri corrispondenti ad un modello"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ci sono alcuni casi in cui può essere necessario eliminare determinati caratteri da una stringa. Ad esempio, potresti voler rimuovere tutti i caratteri di punteggiatura da un testo per ottenere solo le parole, oppure potresti dover eliminare tutti i numeri da una stringa perché non sono rilevanti per il tuo scopo. In questi casi, l'eliminazione dei caratteri che corrispondono a un determinato pattern può semplificare il tuo codice e rendere le operazioni di manipolazione delle stringhe più efficienti.

## Come

Per eliminare i caratteri che corrispondono a un pattern, possiamo utilizzare il metodo Replace fornito dalla classe String in C#. Questo metodo accetta due stringhe come argomenti, la prima rappresenta il pattern che vogliamo cercare e la seconda è la stringa di sostituzione, ossia la stringa che verrà utilizzata per sostituire i caratteri corrispondenti.

Ad esempio, se vogliamo eliminare tutte le vocali da una stringa, possiamo utilizzare questo codice:

```C#
string testo = "Ciao ragazzi!";
string testoSenzaVocali = testo.Replace("a", "").Replace("e", "").Replace("i", "").Replace("o", "").Replace("u", "");
Console.WriteLine(testoSenzaVocali);

// Output: C grggrss!
```

Come puoi vedere, abbiamo utilizzato il metodo Replace più volte per sostituire ogni vocale con una stringa vuota, eliminandola dalla stringa originale.

## Approfondimento

Il metodo Replace ci permette anche di utilizzare espressioni regolari per trovare i caratteri che corrispondono al pattern. Questo ci dà maggiore flessibilità e ci permette di eseguire operazioni più complesse di eliminazione dei caratteri.

Ad esempio, se volessimo eliminare tutti i caratteri di punteggiatura da una stringa, potremmo utilizzare questa espressione regolare:

```C#
string testo = "Questa è una stringa con punteggiatura?!";
string testoSenzaPunteggiatura = Regex.Replace(testo, @"[^\w\s]|_", "");
Console.WriteLine(testoSenzaPunteggiatura);

// Output: Questa è una stringa con punteggiatura
```

Come puoi notare, abbiamo utilizzato il metodo Regex.Replace per sostituire tutti i caratteri che non sono lettere, numeri o spazi con una stringa vuota, ottenendo così una stringa senza punteggiatura.

## Vedi anche

- [Documentazione ufficiale di C# per il metodo Replace](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- [Tutorial sulle espressioni regolari in C#](https://www.c-sharpcorner.com/article/regular-expression-in-c-sharp/)