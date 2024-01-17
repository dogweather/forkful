---
title:                "Utilizzo delle espressioni regolari"
html_title:           "C#: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Che cosa e perché?
Le espressioni regolari, o regex, sono una serie di caratteri che vengono utilizzati per identificare determinati pattern all'interno di una stringa di testo. I programmatori le utilizzano per semplificare e velocizzare l'analisi e la manipolazione di dati testuali.

## Come fare:
Per utilizzare le espressioni regolari in C#, è necessario utilizzare la classe Regex e i relativi metodi. Ecco un esempio di codice che cerca una parola all'interno di una stringa e la sostituisce con un'altra:

```C#
var regex = new Regex(@"\bApple\b"); // crea una nuova istanza della classe Regex con il pattern da cercare
string input = "I love apples!"; // stringa di input
string output = regex.Replace(input, "oranges"); // sostituisce la parola "apples" con "oranges"
Console.WriteLine(output); // stampa "I love oranges!"
```

## Approfondimenti:
Le espressioni regolari esistono da molto tempo, risalendo ai primi anni della programmazione. Negli anni, sono state implementate in molti linguaggi di programmazione, tra cui C#. Tuttavia, ci sono anche alternative come la libreria String.RegularExpressions di .NET Framework e la classe Pattern di Java.

Le espressioni regolari sono basate su un linguaggio formale, chiamato teoria dei linguaggi formali. Inoltre, ci sono numerosi siti web, come Regex101 e Regexr, che consentono di testare ed esplorare i pattern delle espressioni regolari.

## Vedi anche:
- [Microsoft: Espressioni regolari](https://docs.microsoft.com/it-it/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Wikipedia: Espressioni regolari](https://it.wikipedia.org/wiki/Espressione_regolare)
- [Regex101](https://regex101.com/)
- [Regexr](https://regexr.com/)