---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Elm: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Le regular expression, o espressioni regolari, sono strumenti potenti utilizzati dai programmatori per manipolare e analizzare testi e stringhe di caratteri. Spesso abbreviate come "regex", queste espressioni consentono di cercare e sostituire parti specifiche di un testo utilizzando schemi di ricerca ben definiti. Ciò consente ai programmatori di automatizzare task comuni come la convalida delle informazioni di input o la ricerca di parole specifiche in un documento. Usando le regular expression, è possibile scrivere codice più conciso ed efficiente rispetto ai metodi tradizionali di manipolazione delle stringhe.

## Come fare:

Utilizzare regex in Elm è molto semplice. Basta importare il modulo `Regex` e utilizzare la funzione `Regex.find` per cercare una corrispondenza all'interno di una stringa. Ad esempio:

```
import Regex

Regex.find (Regex.regex "elmo") "Elm è un linguaggio di programmazione fantastico!"
```
Questo codice ritornerà un valore di tipo `Maybe Regex.Match` che conterrà i dettagli della corrispondenza trovata, se presente. È anche possibile utilizzare la funzione `Regex.replace` per sostituire parti del testo con un'altra stringa, come mostrato nell'esempio seguente:

```
import Regex

Regex.replace (Regex.regex "-") "01-01-2021" (always "/")
```
In questo caso, il codice ritornerà la stringa "01/01/2021" dopo aver sostituito il carattere "-" con "/".

## Approfondimento:

Le regular expression hanno una lunga storia. Sono state inventate negli anni '50 da Stephen Kleene come strumento per descrivere i linguaggi formali. Negli anni '70, Ken Thompson le ha introdotte nel linguaggio di programmazione Unix, rendendole uno strumento fondamentale per la manipolazione dei testi nei sistemi operativi. Esistono anche alternative come gli algoritmi di ricerca di pattern, ma le regular expression sono ampiamente utilizzate e supportate da molti linguaggi di programmazione, tra cui Elm.

## Vedi anche:

- Documentazione Elm Regex: https://package.elm-lang.org/packages/elm/regex/latest/
- Tutorial Regex su MDN: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
- Libreria Java per regular expression: https://docs.oracle.com/javase/tutorial/essential/regex/