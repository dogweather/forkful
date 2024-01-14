---
title:                "Elm: Ricerca e sostituzione di testo."
simple_title:         "Ricerca e sostituzione di testo."
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La ricerca e la sostituzione di testo sono attività comuni nella programmazione, che possono aiutare a risparmiare tempo ed evitare errori. Con Elm, puoi facilmente automatizzare questo processo e ottenere un codice più pulito e leggibile.

## Come Fare

Per eseguire la ricerca e la sostituzione di testo in un file Elm, segui questi semplici passaggi:

```Elm
-- Definisci una funzione che accetta una stringa e sostituisce tutte le occorrenze
-- di una parola o frase specifica con un'altra parola o frase.

replaceWord : String -> String -> String -> String
replaceWord text word newWord =
  String.replace word newWord text

-- Chiama la funzione con i parametri desiderati.

replaceWord "Questa è una frase di esempio." "frase" "stringa"
-- Output: "Questa è una stringa di esempio."
```

Puoi anche utilizzare la funzione `replace` della libreria `String` per sostituire tutte le occorrenze di una stringa con un'altra stringa, senza dover creare una funzione personalizzata.

```Elm
import String

-- Utilizza la funzione `replace` per sostituire la parola "mondo" con "persona" nella frase.

String.replace "mondo" "persona" "Ciao mondo!"
-- Output: "Ciao persona!"
```

## Approfondimento

A volte, potresti voler effettuare una ricerca e sostituzione su una serie di file o programmi Elm. In questo caso, puoi utilizzare uno strumento come `elm-sed` per automatizzare il processo. `elm-sed` è un utility che utilizza l'IDE di Elm per effettuare ricerca e sostituzione su più file contemporaneamente.

Puoi installare `elm-sed` utilizzando il gestore di pacchetti npm:

```shell
npm install -g elm-sed
```

Una volta installato, puoi eseguirlo su una directory contenente i tuoi file Elm:

```shell
elm-sed "Ciao mondo" "Salve mondo" *.elm
```

Questo esempio effettuerà una ricerca della stringa "Ciao mondo" nei file `.elm` presenti nella directory e sostituirà ogni occorrenza con la stringa "Salve mondo".

## Vedi Anche

- [Documentazione String.replace](https://package.elm-lang.org/packages/elm/core/latest/String#replace)
- [Elm-sed su GitHub](https://github.com/mimosa-editor/elm-sed)