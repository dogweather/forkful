---
title:    "Elm: Estrazione di sottostringhe"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore di Elm o sei solo curioso di imparare questo linguaggio funzionale, probabilmente hai incontrato la necessità di estrarre porzioni di una stringa. Questo può essere utile per manipolare i dati e creare output personalizzati. In questo articolo, ti mostrerò come effettuare l'estrazione di substring in Elm.

## Come fare

Per estrarre una sottostringa in Elm, puoi utilizzare la funzione `String.slice start end string`. Questa funzione prende come argomenti l'indice di inizio, l'indice di fine e la stringa dalla quale estrarre la sottostringa. Ad esempio:

```Elm
String.slice 0 3 "ciao mondo" -- output: "cia"
```

Puoi anche utilizzare un solo argomento per specificare l'indice di inizio e la sottostringa verrà estratta fino alla fine. Ad esempio:

```Elm
String.slice 3 "ciao mondo" -- output: "o mondo"
```

Puoi anche utilizzare valori negativi per gli indici di inizio e fine, che conta all'indietro dalla fine della stringa. Ad esempio:

```Elm
String.slice -3 -1 "ciao mondo" -- output: "ndo"
```

Inoltre, puoi utilizzare la funzione `String.left n string` per estrarre i primi `n` caratteri di una stringa e `String.right n string` per estrarre gli ultimi `n` caratteri.

## Approfondimento

Estrarre sottostringhe è utile non solo per manipolare i dati, ma anche per validare input da parte degli utenti. Ad esempio, puoi utilizzare la funzione `String.slice` per verificare che una password abbia una lunghezza specifica o che un codice fiscale abbia la giusta struttura. Inoltre, puoi combinare le funzioni `String.slice` e `String.contains` per estrarre e verificare una sottostringa contemporaneamente.

## Vedi anche

- Documentazione ufficiale sulle funzioni di manipolazione delle stringhe in Elm: https://package.elm-lang.org/packages/elm/core/latest/String
- Blog post su come utilizzare le funzioni di manipolazione delle stringhe in Elm: https://dev.to/kdow/elm-string-and-strings-4ani