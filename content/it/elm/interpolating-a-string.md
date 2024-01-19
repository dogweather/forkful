---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolazione delle Stringhe in Elm

## Cos'è e Perché?

L'interpolazione delle stringhe è un metodo per inserire variabili dentro una stringa. I programmatori lo fanno per creare stringhe dinamiche senza interrompere il flusso del loro codice.

## Come si fa:

In Elm, l'interpolazione delle stringhe si fa con l'operatore di concatenazione (`++`). Ecco un esempio:

```Elm
nome = "Luca"
saluto = "Ciao, " ++ nome ++ "!"

-- Il tuo output sarà: "Ciao, Luca!"
```

## Una Visione Più Approfondita

Mentre altri linguaggi hanno funzionalità di interpolazione di stringhe integrate, Elm utilizza l'operatore di concatenazione. Questo è coerente con la filosofia del linguaggio di mantenere le cose semplici e prevedibili.

Esistono alternative all'uso dell'operatore di concatenazione, ad esempio, l'utilizzo di funzioni di libreria per formattare le stringhe. Tuttavia, questo tende a complicare il codice e non è generalmente consigliato.

Dal punto di vista dell'implementazione, tenete a mente che l'operatore di concatenazione crea una nuova stringa. Quindi, l'interpolazione di stringhe frequenti e su larga scala può avere un impatto sulle prestazioni.

## Per Ulteriori Informazioni

Se desiderate saperne di più su come lavorare con le stringhe in Elm, potete esplorare le seguenti risorse:

1. [Guida ufficiale di Elm sulla String](https://package.elm-lang.org/packages/elm/core/latest/String)
2. [Elm: Concatenazione della Stringa](https://elm-lang.org/examples/string-concat)
3. [Stringhe in Elm - Scuola di Elm (Inglese)](https://elmprogramming.com/strings.html)