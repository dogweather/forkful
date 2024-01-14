---
title:    "Elm: Ottenere la data corrente"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando stiamo scrivendo codice in Elm, abbiamo bisogno di visualizzare la data corrente. Ci può essere una funzionalità che deve essere attivata solo in una determinata data o semplicemente vogliamo aggiungere la data ai nostri log di debug. In questo articolo vedremo come ottenere la data corrente in Elm.

## Come fare

Per ottenere la data corrente in Elm, possiamo utilizzare la funzione `Date.now`, che ci darà un valore di tipo `Date` rappresentante il momento in cui viene chiamata.

Un'importante cosa da notare è che la funzione `Date.now` è dipendente dal tempo di sistema del dispositivo che esegue il programma. Se il dispositivo ha una data o un orario errato, questo influenzerà anche il valore restituito da `Date.now`.

```Elm
import Date exposing (Date, now)

main =
    let
        currentDate =
            now
    in
    currentDate
```

Questo esempio semplicemente salva il valore restituito dalla funzione `now` in una variabile chiamata `currentDate` e successivamente lo restituisce come model dell'app. Nota che la data corrisponderà all'orario di sistema del dispositivo al momento dell'esecuzione del programma.

Possiamo anche utilizzare la funzione `Date.fromDateParts` per creare una data specifica impostando l'anno, il mese e il giorno.

```Elm
import Date exposing (Date, fromDateTime)

main =
    let
        specificDate =
            Date.fromDateParts 2020 10 31
    in
    specificDate
```

Questo esempio restituisce una data rappresentante il 31 ottobre 2020. È utile per creare date specifiche per scopi di testing o per impostare una data di inizio di un progetto.

## Approfondimento

Se vogliamo ottenere informazioni più precise sulla data corrente, possiamo utilizzare le funzioni di manipolazione delle date offerte dal modulo `Date`. Ad esempio, per ottenere il giorno della settimana corrente possiamo utilizzare la funzione `Date.dayOfWeek`.

```Elm
import Date exposing (Date, dayOfWeek, now)

main =
    let
        currentDate =
            now

        dayOfWeek =
            Date.dayOfWeek currentDate
    in
    dayOfWeek
```

Il valore restituito sarà un intero da 1 a 7, corrispondente ai giorni della settimana (1 per domenica, 7 per sabato).

## Vedi anche

- Documentazione del modulo Date in Elm: https://package.elm-lang.org/packages/elm/time/latest/Date
- Articolo su come gestire le date in Elm: https://dev.to/andrewmacmurray/handling-dates-in-elm-1hkc