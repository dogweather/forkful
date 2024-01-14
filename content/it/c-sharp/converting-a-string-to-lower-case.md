---
title:                "C#: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Le stringhe sono uno dei tipi di dati più comuni utilizzati nella programmazione, e spesso è necessario manipolarle per adattarle alle nostre esigenze. Una situazione comune è quella in cui vogliamo confrontare due stringhe senza tener conto delle loro maiuscole o minuscole. In questo caso, dobbiamo convertire le stringhe in minuscolo per poterle confrontare correttamente. Quindi, imparare come convertire una stringa in minuscolo è estremamente utile per eseguire operazioni di confronto e ricerca nella programmazione.

## Come

```C#
string myString = "QUESTA È UNA STRINGA DA CONVERTIRE";

// Utilizzando il metodo ToLower()
Console.WriteLine(myString.ToLower());
// Output: questa è una stringa da convertire

// Utilizzando l'operatore di assegnazione
myString = myString.ToLower();
Console.WriteLine(myString);
// Output: questa è una stringa da convertire
```

In C#, esistono diversi modi per convertire una stringa in minuscolo. Uno dei metodi più comuni è l'utilizzo del metodo `ToLower()`, che restituisce una nuova stringa convertita in minuscolo. Possiamo anche utilizzare l'operatore di assegnazione (`=`) per convertire e assegnare contemporaneamente la nuova stringa in una variabile. Questo ci permette di risparmiare qualche riga di codice.

## Deep Dive

È importante notare che la conversione in minuscolo di una stringa dipende dalla cultura specifica del sistema operativo. Ciò significa che la stringa convertita può essere diversa in base a dove viene eseguito il codice. Per evitare problemi di questo genere, si consiglia di utilizzare esplicitamente il metodo `ToLower()` anziché l'operatore di assegnazione.

Inoltre, è importante prestare attenzione alle differenze tra le lettere maiuscole e minuscole nelle diverse lingue, poiché possono influire sulla conversione della stringa. Ad esempio, in alcune lingue esistono caratteri speciali che possono essere trasformati in lettere maiuscole o minuscole. Ciò può influenzare il risultato della conversione in minuscolo e deve essere considerato nel codice.

## Vedi Anche

- [Stringa (C#)](https://docs.microsoft.com/it-it/dotnet/csharp/language-reference/builtin-types/string)
- [CultureInfo Class (System.Globalization)](https://docs.microsoft.com/it-it/dotnet/api/system.globalization.cultureinfo?view=netcore-3.1)
- [ToUpper() Method (System.String)](https://docs.microsoft.com/it-it/dotnet/api/system.string.toupper?view=netcore-3.1)