---
title:    "Elm: Calcolare una data nel futuro o nel passato"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile per diverse ragioni, come ad esempio programmare un evento o tenere traccia di una data importante.

## Come fare

Per calcolare una data nel futuro o nel passato con Elm, è necessario utilizzare i moduli `Time` e `Date`.

Iniziamo importando i moduli necessari e definendo una costante che rappresenta il numero di millisecondi in un giorno:

```Elm
import Time exposing (..)
import Date exposing (..)

millisecondiInUnGiorno : Int
millisecondiInUnGiorno = 86400000
```

Per calcolare una data futura a partire dalla data corrente, possiamo utilizzare la funzione `add` del modulo `Time` e specificare il numero di millisecondi da aggiungere alla data corrente:

```Elm
Time.now |> add millisecondiInUnGiorno |> Time.toMillis |> Time.millisToPosix |> Date.fromPosix
```

Questo ci darà come risultato la data di domani.

Un altro modo per calcolare una data futura è utilizzare la funzione `addDays` del modulo `Date`:

```Elm
fromExternalIsoDate "2021-10-01" |> addDays 7
```

Questo ci darà come risultato la data di una settimana dopo il primo ottobre 2021.

Per calcolare una data nel passato, possiamo utilizzare la funzione `subtract` del modulo `Time` e specificare il numero di millisecondi da sottrarre dalla data corrente:

```Elm
Time.now |> subtract millisecondiInUnGiorno |> Time.toMillis |> Time.millisToPosix |> Date.fromPosix
```

Questa volta otterremo come risultato la data di ieri.

Possiamo anche utilizzare la funzione `subDays` del modulo `Date` per calcolare una data nel passato:

```Elm
fromExternalIsoDate "2021-10-08" |> subDays 5
```

Questo ci darà come risultato la data di cinque giorni prima dell'otto ottobre 2021.

## Approfondimenti

Calcolare una data nel futuro o nel passato può implicare anche la gestione dei fusi orari e dei giorni bisestili. Per maggiori informazioni su questi argomenti, si consiglia di consultare la documentazione dei moduli `Time` e `Date` di Elm.

## Vedi anche

- [Documentazione del modulo Time di Elm](https://package.elm-lang.org/packages/elm/time/latest/)
- [Documentazione del modulo Date di Elm](https://package.elm-lang.org/packages/elm/date/latest/)