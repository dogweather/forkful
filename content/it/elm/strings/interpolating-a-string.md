---
date: 2024-01-20 17:50:47.957806-07:00
description: "Interpolare una stringa significa inserire variabili o espressioni all'interno\
  \ di una stringa di testo. I programmatori lo fanno per creare messaggi\u2026"
lastmod: '2024-02-25T18:49:41.205013-07:00'
model: gpt-4-1106-preview
summary: "Interpolare una stringa significa inserire variabili o espressioni all'interno\
  \ di una stringa di testo. I programmatori lo fanno per creare messaggi\u2026"
title: Interpolazione di una stringa
---

{{< edit_this_page >}}

## What & Why?
Interpolare una stringa significa inserire variabili o espressioni all'interno di una stringa di testo. I programmatori lo fanno per creare messaggi dinamici o per costruire stringhe con valori che cambiano a runtime.

## How to:
Elm non ha interpolazione di stringa incorporata, ma possiamo concatenare stringhe con l'operatore `++`.

```Elm
name = "Mondo"
greeting = "Ciao, " ++ name ++ "!"

-- Output: "Ciao, Mondo!"
```

Per valori non stringa, usiamo la funzione `String.fromInt` per convertire:

```Elm
age = 30
message = "Hai " ++ String.fromInt(age) ++ " anni."

-- Output: "Hai 30 anni."
```

## Deep Dive
Elm rimane fedele alla sua filosofia di semplicità, evitando complessità inutili come l'interpolazione di stringhe di altri linguaggi come JavaScript. Questo può sembrare un'inconvenienza, ma mantiene il linguaggio pulito e prevedibile. Alternative includono l'usare la concatenazione, come mostrato sopra, o funzioni che formattano e costruiscono stringhe. Un esempio è `String.concat`, che unisce una lista di stringhe:

```Elm
String.concat ["Ciao, ", name, "!"]
```

Un'altra operazione comune è la costruzione di stringhe contenenti rappresentazioni di diverse tipologie di dati. In Elm, si fa ricorso a funzioni di conversione come `String.fromInt`, `String.fromFloat`, `String.fromBool`, e così via.

Prima della versione attuale, Elm aveva sperimentato con un operatore di interpolazione simile a quello di altri linguaggi, ma è stato rimosso per mantenere la coerenza e l'affidabilità del linguaggio.

## See Also
- Documentazione ufficiale Elm per la lavorazione delle Stringhe: [Elm String](http://package.elm-lang.org/packages/elm/core/latest/String)
