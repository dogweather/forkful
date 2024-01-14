---
title:    "Elm: Confrontare due date"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Se stai sviluppando un'applicazione in Elm, potresti trovarti nella necessità di confrontare due date per controllare se una data è precedente o successiva all'altra. In tal caso, è importante sapere come gestire questa operazione correttamente. In questo articolo, vedremo come confrontare due date in Elm in modo semplice ed efficace.

## Come Fare

Per confrontare due date in Elm, è possibile utilizzare la funzione `Time.compare` che accetta due valori di tipo `Time.Posix` come parametri. Questa funzione restituisce uno dei seguenti valori:

- `LT` se la prima data è precedente alla seconda
- `EQ` se le due date sono uguali
- `GT` se la prima data è successiva alla seconda

Vediamo un esempio di codice che utilizza questa funzione:

```
Elm
import Time

postDate : Time.Posix
postDate = Time.fromCalendarDate 2021 5 11 0 0 0

currentDate : Time.Posix
currentDate = Time.now

comparison : Time.Order
comparison = Time.compare postDate currentDate

main =
    case comparison of
        Time.LT ->
            Html.text "La data dell'articolo è precedente alla data attuale"

        Time.EQ ->
            Html.text "Gli articoli sono stati pubblicati nella stessa data"

        Time.GT ->
            Html.text "La data dell'articolo è successiva alla data attuale"
```

In questo esempio, stiamo confrontando la data in cui è stato pubblicato un articolo (`postDate`) con la data attuale (`currentDate`), ottenuta utilizzando la funzione `Time.now`. A seconda del risultato ottenuto, viene visualizzato un messaggio diverso all'utente.

## Approfondimento

Se vuoi approfondire ulteriormente il confronto tra due date in Elm, puoi utilizzare altri moduli come `Time.Date` e `Time.Calendar` per ottenere numeri interi corrispondenti al giorno, al mese o all'anno di una data. Puoi quindi utilizzare questi numeri per confrontare le date in modo più dettagliato e personalizzato.

## Vedi Anche

- Documentazione ufficiale di Elm sul confronto delle date: https://package.elm-lang.org/packages/elm/time/latest/Time#compare
- Utilizzo dei moduli Time.Date e Time.Calendar: https://guide.elm-lang.org/interop/dates_and_times.html#comparing-dates