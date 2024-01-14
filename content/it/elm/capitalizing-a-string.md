---
title:                "Elm: Capitalizzare una stringa"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per voler capitalizzare una stringa in Elm. Puoi farlo per estetica, per uniformità con altre stringhe nel tuo programma o per manipolare la visualizzazione di testo nel tuo progetto. Ad esempio, potresti voler capitalizzare il nome di una persona in una form di registrazione.

## Come Fare

Per capitalizzare una stringa in Elm, puoi utilizzare la funzione `String.toUpper` che prende una stringa come input e restituisce la stessa stringa con tutti i caratteri in maiuscolo. Vediamo un esempio di come utilizzarla all'interno di un programma Elm:

```Elm
import String exposing (toUpper)

stringaIniziale = "ciao a tutti!"

stringaCapitalizzata = toUpper stringaIniziale

-- Output: "CIAO A TUTTI!"
```

Se hai bisogno di capitalizzare solo la prima lettera di una stringa, puoi utilizzare la funzione `String.capitalize` che converte solo il primo carattere in maiuscolo. Ecco un esempio di come utilizzarla:

```Elm
import String exposing (capitalize)

stringaIniziale = "ciao a tutti!"

stringaCapitalizzata = capitalize stringaIniziale

-- Output: "Ciao a tutti!"
```

## Approfondimenti

Capitalizzare una stringa può sembrare un'operazione semplice, ma ci sono alcune cose da considerare. Ad esempio, la funzione `toUpper` non funziona su stringhe che contengono caratteri speciali o numeri. Inoltre, se la tua stringa contiene già lettere maiuscole, la funzione non farà alcuna modifica. Pertanto, è importante esaminare attentamente i dati di input e assicurarsi di gestire tutti i casi possibili nel tuo codice.

## Vedi Anche

- Documentazione della funzione `String.toUpper`: https://package.elm-lang.org/packages/elm/core/latest/String#toUpper
- Documentazione della funzione `String.capitalize`: https://package.elm-lang.org/packages/elm/core/latest/String#capitalize
- Altri metodi per manipolare le stringhe in Elm: https://guide.elm-lang.org/strings/