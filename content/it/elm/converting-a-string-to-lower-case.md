---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
La conversione di una stringa in caratteri minuscoli è l'operazione di trasformare tutti i caratteri alfabetici di una stringa da maiuscoli a minuscoli. I programmatori la utilizzano per assicurarsi che i dati siano uniformi e per evitare problemi di confronto tra stringhe.

## Come fare:
Ecco un esempio di come convertire una stringa in minuscolo in Elm.

```Elm
import String

main =
  print (String.toLower "Buongiorno, Signore!")
```

L'output sarà: "buongiorno, signore!".

## Più nel dettaglio:
La funzione `toLower` appartiene al modulo `String` in Elm e fa parte delle funzionalità standard del linguaggio dalla versione 0.19. Non ci sono alternative native dirette in Elm da quando è stato introdotto `String.toLower`, ma prima era necessario utilizzare `Char.toLower` in combinazione con `String.map`. 

Per quanto riguarda i dettagli di implementazione, `String.toLower` in Elm internamente utilizza una implementazione JavaScript che esegue l'operazione per ogni carattere della stringa. Elm garantisce che questo processo sia sicuro in termini di tipi e gestisce eventuali errori.

## Vedi anche:
Per approfondire:
1. Documentazione di Elm sul modulo ['String'](https://package.elm-lang.org/packages/elm/core/latest/String)
2. Il repo GitHub di ['Elm'](https://github.com/elm/compiler) compreso il codice sorgente di `String.toLower`
3. Approfondimenti sulle funzioni di stringa in generale in [Elm programming guide](https://guide.elm-lang.org/)