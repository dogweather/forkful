---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che Cos’è e Perché?

Convertire una stringa in caratteri minuscoli significa modificare ogni lettera maiuscola in minuscola. Questo viene utilizzato dai programmatori per normalizzare il testo, consentendo confronti di stringhe non sensibili alle maiuscole o minuscole.

## Come fare:

Ecco come puoi convertire una stringa in minuscolo in C#:

```C#
string s = "Ciao, MONDO!";
string lower = s.ToLowerInvariant();
Console.WriteLine(lower);
```

L'output sarà:

```C#
"ciao, mondo!"
```

## Approfondimento:

Molte lingue di programmazione, inclusa C#, offrono metodi per convertire le stringhe in minuscolo. In C#, ToLowerInvariant() è un metodo comune per convertire tutti i caratteri alfanumerici di una stringa in minuscolo. 

Il metodo ToLowerInvariant() viene utilizzato per ignorare le differenze di localizzazione. Ad esempio, in alcune lingue, certi caratteri possono non avere un equivalente minuscolo. In questi casi, ToLowerInvariant() restituisce il carattere originale.

In alternativa, C# offre anche la funzione ToLower() che considera le impostazioni di localizzazione correnti. Tuttavia, in un contesto globale, l'uso di ToLowerInvariant() è generalmente preferito.

Infine, è importante sottolineare che entrambi i metodi creano una nuova stringa anziché modificare quella esistente. C# tratta le stringhe come immutabili, il che significa che una volta creata una stringa, il suo valore non può essere modificato.

## Per ulteriori informazioni:

- [Documentazione Microsoft sulla funzione ToLowerInvariant()](https://docs.microsoft.com/dotnet/api/system.string.tolowerinvariant?view=net-5.0)
- [Documentazione Microsoft sulla funzione ToLower()](https://docs.microsoft.com/dotnet/api/system.string.tolower?view=net-5.0)
- [Microsoft: I simboli di stringa in C#](https://docs.microsoft.com/dotnet/csharp/programming-guide/strings/)