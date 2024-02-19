---
aliases:
- /it/c-sharp/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:08.491346-07:00
description: "Convertire una stringa in minuscolo significa trasformare tutti i caratteri\
  \ alfabetiche di quella stringa in lettere minuscole. I programmatori fanno\u2026"
lastmod: 2024-02-18 23:08:55.875043
model: gpt-4-1106-preview
summary: "Convertire una stringa in minuscolo significa trasformare tutti i caratteri\
  \ alfabetiche di quella stringa in lettere minuscole. I programmatori fanno\u2026"
title: Conversione di una stringa in minuscolo
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Convertire una stringa in minuscolo significa trasformare tutti i caratteri alfabetiche di quella stringa in lettere minuscole. I programmatori fanno questo per uniformare i dati testuali, facilitando la comparazione e la ricerca all'interno di testi, ignorando le differenze tra maiuscole e minuscole.

## How to: (Come fare:)
In C#, per convertire una stringa in minuscolo, usiamo il metodo `.ToLower()` o `.ToLowerInvariant()`. Ecco come funziona:

```C#
string originalString = "CIAO Mondo!";
string lowerCaseString = originalString.ToLower();

Console.WriteLine(lowerCaseString);
// Output: ciao mondo!
```

E per usare `.ToLowerInvariant()`:

```C#
string originalString = "CIAO Mondo!";
string lowerCaseInvariantString = originalString.ToLowerInvariant();

Console.WriteLine(lowerCaseInvariantString);
// Output: ciao mondo!
```

La differenza? `.ToLower()` tiene conto delle impostazioni locali (cultura) del sistema, mentre `.ToLowerInvariant()` ignora la cultura e segue le regole di standardizzazione internazionale.

## Deep Dive (Approfondimento)
Prima dell’introduzione di Unicode, la conversione in minuscolo si limitava ai range di caratteri ASCII. Con Unicode, è diventato più complesso, perché ogni lingua ha regole specifiche per le maiuscole e le minuscole.

Oltre a `.ToLower()` e `.ToLowerInvariant()`, esiste `String.ToLower(CultureInfo)` che permette di specificare la cultura da usare per la conversione. Questo è utile quando si lavora con testi multilingua.

L'implementazione interna di `.ToLower()` sfrutta le tabelle di mappatura di caratteri di .NET per determinare equivalenti in minuscolo. È interessante notare che non tutte le lingue hanno un concetto di maiuscole/minuscole, quindi la conversione ha senso principalmente per lingue come l'inglese, l'italiano, ecc.

## See Also (Vedi anche)
- Documentazione ufficiale Microsoft su `.ToLower()` e `.ToLowerInvariant()`: [String.ToLower Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-6.0)
- Documentazione su Unicode e il suo impatto sulla gestione delle stringhe in C#: [Unicode and .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/character-encoding)
- Informazioni sulla gestione delle culture in .NET: [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo?view=net-6.0)
