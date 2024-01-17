---
title:                "Estrazione di sottostringhe"
html_title:           "C#: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

L'estrazione di sottostringhe è il processo di selezione di una parte di una stringa più grande. I programmatori spesso lo fanno per ottenere solo una porzione delle informazioni di una stringa o per manipolare i dati in modo più efficiente.

## Come fare:

Per estrarre una sottostringa in C#, possiamo utilizzare il metodo ```Substring()``` della classe string. Vediamo un esempio:

```c#
string frase = "Benvenuti in questo articolo!";
string sottostringa = frase.Substring(10, 7); // estrae la sottostringa "questo"
Console.WriteLine(sottostringa); // output: questo
```

Qui stiamo utilizzando il metodo ```Substring()``` per estrarre una sottostringa dalla posizione 10 della stringa fino a 7 caratteri dopo. Possiamo anche specificare solo la posizione di inizio e ottenere la sottostringa fino alla fine della stringa:

```c#
string ulterioriInformazioni = frase.Substring(22); // estrae "articolo!"
```

Notare che le posizioni all'interno di una stringa iniziano da 0.

## Approfondimenti:

L'idea di estrarre sottostringhe esiste da molto tempo ed è stata utilizzata in molti linguaggi di programmazione diversi. In C#, ci sono anche altri modi per estrarre sottostringhe, come utilizzare i metodi ```Split()``` e ```Replace()``` o i pattern di ricerca tramite espressioni regolari.

Inoltre, l'estrazione di sottostringhe è spesso utilizzata in congiunzione con altri metodi e funzioni per manipolare e analizzare le stringhe. Ad esempio, possiamo utilizzare la sottostringa ottenuta per cercare una corrispondenza specifica tramite il metodo ```Contains()``` o per convertire i dati in un tipo diverso utilizzando i metodi di casting.

## Vedi anche:

- Documentazione ufficiale su ```Substring()``` in C#: [https://docs.microsoft.com/en-us/dotnet/api/system.string.substring](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- Esempi di utilizzo di sottostringhe in C#: [https://www.geeksforgeeks.org/c-sharp-string-substring-method/](https://www.geeksforgeeks.org/c-sharp-string-substring-method/)
- Approfondimenti sulle espressioni regolari in C#: [https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)