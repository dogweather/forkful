---
title:    "Elm: Calcolare una data nel futuro o nel passato"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Non c'è niente di più frustrante di dover calcolare una data nel futuro o nel passato manualmente. Fortunatamente, con Elm, è possibile automatizzare questo processo. In questo articolo, esploreremo come calcolare date in modo efficiente utilizzando Elm.

## Come fare

Per calcolare una data nel futuro o nel passato, è necessario utilizzare la libreria standard `Time` di Elm. Iniziamo importando questa libreria all'inizio del nostro file:

```Elm
import Time
```

Per calcolare una data nel futuro, possiamo utilizzare la funzione `Time.fomDate` che prende come argomenti l'anno, il mese e il giorno desiderati e restituisce una stringa formattata in modo leggibile:

```Elm
let
  futureDate = Time.fromDate 2021 10 31
in
  "La data nel futuro è: " ++ futureDate -- Output: "La data nel futuro è: dom 31 ott 2021" 
```

Per calcolare una data nel passato, possiamo utilizzare la funzione `Time.toTime` che prende come argomento una stringa formattata in modo leggibile e restituisce una data in millisecondi dal 1° gennaio 1970:

```Elm
let
  pastDate = Time.toTime "mer 31 mar 1993"
in
  "La data nel passato è: " ++ pastDate -- Output: "La data nel passato è: -781321200000"
```

## Approfondimento

È possibile calcolare date in modo ancora più preciso utilizzando le funzioni di `Time` come `year`, `month` e `day`. Inoltre, è possibile utilizzare il modulo `Time.Calendar` per ottenere il numero di giorni in un dato mese o per verificare se un anno è bisestile.

See Also

- Documentazione di Time https://package.elm-lang.org/packages/elm/time/latest/Time
- Tutorial di Elm https://guide.elm-lang.org/