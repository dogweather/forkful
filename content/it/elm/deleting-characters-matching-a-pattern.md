---
title:                "Elm: Eliminare caratteri corrispondenti a un modello"
simple_title:         "Eliminare caratteri corrispondenti a un modello"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Il motivo principale per cui si potrebbe voler eliminare dei caratteri che corrispondono ad un determinato pattern è per la pulizia dei dati. Ciò può essere particolarmente utile quando si lavora con grandi quantità di testo o quando si deve estrarre informazioni specifiche da una stringa.

## Come

Per eliminare i caratteri che corrispondono ad un pattern in Elm, possiamo utilizzare la funzione `Regex.replace` fornita dal pacchetto `elm/regex`. Questa funzione prende tre argomenti: il pattern di ricerca, la stringa in cui cercare e la stringa di sostituzione.

Ad esempio, se volessimo eliminare tutte le vocali da una stringa, potremmo scrivere il seguente codice:

```Elm
import Regex

frase = "Questa è una frase di esempio"
pattern = Regex.regex "[aeiou]"

output = Regex.replace pattern frase ""
```

In questo caso, il risultato sarebbe una stringa senza vocali: "Qst è n frs d msp".

Possiamo anche utilizzare caratteri speciali nel nostro pattern, come ad esempio `\w` per cercare tutti i caratteri alfanumerici o `\s` per cercare tutti gli spazi bianchi.

## Deep Dive

C'è molto di più da esplorare quando si tratta di eliminare caratteri che corrispondono ad un pattern in Elm. Ad esempio, la funzione `Regex.replace` ci permette di passare una funzione come stringa di sostituzione, che significa che possiamo decidere dinamicamente come sostituire i caratteri corrispondenti. Inoltre, la libreria `Regex` offre anche altre funzioni utili come `Regex.contains` e `Regex.find`, che ci permettono di cercare estrarre contenuti basati su un pattern.

## Vedi anche

- [Documentazione delle funzioni per l'espressione regolare di Elm](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Tutorial sull'utilizzo delle espressioni regolari in Elm](https://thoughtbot.com/blog/elm-regex-cheatsheet)