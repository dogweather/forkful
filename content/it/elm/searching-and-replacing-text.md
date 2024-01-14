---
title:    "Elm: Cercare e sostituire testo"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La ricerca e sostituzione di testo è una pratica comune nella programmazione, in particolare quando si lavora con grandi quantità di dati. Con Elm, è possibile automatizzare questo processo per risparmiare tempo e ridurre gli errori umani.

## Come fare

Per eseguire la ricerca e sostituzione di testo in Elm, si può utilizzare il comando `String.replace` che accetta tre argomenti: il testo da cercare, il testo di sostituzione e la stringa di input. Ad esempio:

```Elm
String.replace "ciao" "hello" "ciao mondo"
```

Questa funzione restituirà la stringa "hello mondo", sostituendo ogni occorrenza di "ciao" con "hello".

È inoltre possibile utilizzare espressioni regolari per una maggiore flessibilità nelle ricerche di testo. Ad esempio, per sostituire tutte le vocali di una parola con asterischi, si può utilizzare il seguente codice:

```Elm
RegExp.replace (RegExp.Regex "^[aeiouy]") (\_ -> "*") "elm"
```

## Approfondimento

Esistono alcune considerazioni importanti da tenere a mente quando si effettua la ricerca e sostituzione di testo in Elm. In primo luogo, il comando `String.replace` è case-sensitive, quindi se si cerca di sostituire una parola come "ciao" con "hello", solo "ciao" sarà riconosciuto e sostituito. In secondo luogo, è importante prestare attenzione alla formattazione delle espressioni regolari per ottenere i risultati desiderati.

È inoltre possibile utilizzare la funzione `String.filter` per rimuovere le parti del testo non desiderate prima di effettuare la ricerca e la sostituzione. Questo può essere utile per rimuovere eventuali caratteri di punteggiatura o spazi vuoti che potrebbero interferire con la ricerca.

## Vedi anche

- Documentazione di Elm su `String.replace`: https://package.elm-lang.org/packages/elm/core/latest/String#replace
- Tutorial sulle espressioni regolari in Elm: https://dev.to/elm-yoga/basics-of-regex-in-elm-2md9