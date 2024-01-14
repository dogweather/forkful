---
title:    "Elm: Converting una data in una stringa"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Perché

Spesso ci troviamo ad avere la necessità di convertire una data in una stringa per poterla mostrare all'utente in un formato più comprensibile. In Elm, ci sono diverse funzioni utili che ci permettono di fare questo in modo semplice e preciso.

## Come

Per convertire una data in una stringa, possiamo utilizzare la funzione `toString` del modulo `Date`. Ad esempio, se vogliamo mostrare oggi come stringa nella forma "giorno/mese/anno", possiamo scrivere:

```Elm
Date.toString (Date.fromTime (Time.millisToPosix 0)) "dd/MM/yyyy"
```

Questo ci restituirà una stringa che rappresenta l'ora attuale nella forma specificata. Se vogliamo invece convertire una data specifica, possiamo fare così:

```Elm
Date.toString (Date.fromTime (Time.millisToPosix 1583979600000)) "dd MMMM yyyy"
```

In questo caso, stiamo convertendo la data del 12/03/2020 nella forma "12 marzo 2020".

## Approfondimento

Oltre alla funzione `toString`, il modulo `Date` ci offre anche altre funzioni utili per manipolare le date e le stringhe. Ad esempio, `fromIsoString` ci permette di convertire una stringa di formato ISO-8601 in una data, mentre `toInternetDate` ci consente di ottenere una stringa di formato RFC-1123 a partire da una data.

Inoltre, è possibile specificare diverse opzioni per il formato della stringa di output, come ad esempio la lingua e la zona oraria. È importante sempre leggere la documentazione delle funzioni per utilizzarle correttamente.

## Vedi anche

- Documentazione ufficiale di Elm sul modulo `Date`: https://package.elm-lang.org/packages/elm/time/latest/Date
- Tutorial su come utilizzare il modulo `Date`: https://elmprogramming.com/using-date-picker-inputs.html
- Esempi di utilizzo del modulo `Date` in Elm: https://github.com/rtfeldman/elm-date-extra