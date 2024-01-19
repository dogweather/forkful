---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# L'interpolazione delle stringhe in C#

Il tuo strumento pratico per formattare le stringhe in modo pulito.

## Cos'è e perché?

L'interpolazione delle stringhe è un modo per inserire variabili direttamente nelle stringhe. Lo facciamo per ottenere un codice più pulito e leggibile.

## Come fare:

```C#
string nome = "Mario";
int eta = 30;
string output = $"Ciao {nome}, hai {eta} anni.";

Console.WriteLine(output);  

// Output: Ciao Mario, hai 30 anni.
```
 
'${var}' sostituisce 'var' con il suo valore. Basta con String.Format() laborioso!

## Approfondimento:

L'interpolazione delle stringhe esiste dal C# 6.0. Prima eravamo obbligati a usare la vecchia String.Format(), oppure concatenare con '+'. L'interpolazione richiede meno fatica ed è più intuitiva. Di solito, viene convertita in String.Format() dal compilatore.

```C#
//Old
string output = string.Format("Ciao {0}, hai {1} anni.", nome, eta);

//Older
string output = "Ciao " + nome + ", hai " + eta + " anni.";
```

Osserva la differenza di chiarezza!

## Approfondisci:

[Microsoft - String Interpolation (C# Reference)](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated) 

[DotNetPearls - C# String Interpolation](https://www.dotnetperls.com/string-interpolation)  

C'è sempre spazio per apprendere qualcosa di nuovo. Buona codifica!