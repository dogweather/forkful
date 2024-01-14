---
title:                "Elm: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se stai scrivendo codice in Elm, potresti trovarti nella situazione in cui devi estrarre una sottostringa da una stringa più grande. Questa operazione è comune quando si lavora con dati di input utente o si deve manipolare una stringa prima di elaborarla ulteriormente. In questo caso, conoscere come estrarre una sottostringa è utile per semplificare il tuo codice e ottenere il risultato desiderato.

## Come fare

Per estrarre una sottostringa in Elm, utilizzeremo una funzione chiamata `slice` che accetta tre parametri: la stringa di input, l'indice iniziale e l'indice finale della sottostringa che desideri estrarre. Ad esempio, se volessi estrarre il secondo, terzo e quarto carattere dalla parola "ciao", dovresti scrivere il seguente codice:

```Elm
slice "ciao" 1 4
```

Questo ci restituirebbe "iao" come output. Vale la pena notare che l'indice finale è esclusivo, quindi dobbiamo includere l'indice successivo all'ultimo carattere che vogliamo estrarre.

## Approfondimento

La funzione `slice` può essere utilizzata anche per estrarre le sottostringhe in base alla loro lunghezza invece che agli indici. Ciò significa che possiamo specificare quanti caratteri vogliamo estrarre dalla stringa: il secondo parametro diventa la posizione iniziale e il terzo parametro diventa la lunghezza della sottostringa. Ad esempio:

```Elm
slice "ciao" 1 2
```

Questo ci restituirebbe "ci" come output, poiché stiamo iniziando dalla seconda posizione e stiamo estraendo 2 caratteri. Questo è utile quando non conosciamo gli indici esatti della sottostringa che vogliamo estrarre.

## Vedi anche

- La documentazione ufficiale di Elm sulla funzione `slice`: https://package.elm-lang.org/packages/elm/core/latest/String#slice
- Un articolo di Medium che esplora altre funzioni utili per lavorare con le stringhe in Elm: https://medium.com/swlh/manipulating-strings-in-elm-486b683e4a42