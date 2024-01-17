---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Haskell: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Cosa e Perché?
L'utilizzo di espressioni regolari è uno dei metodi più comuni per la manipolazione dei testi nel mondo della programmazione. Consiste nell'utilizzare uno schema di caratteri per identificare determinati pattern all'interno di una stringa. I programmatori lo utilizzano per semplificare il processo di ricerca, estrazione o sostituzione di parti specifiche all'interno di un testo.

# Come fare:
Per utilizzare espressioni regolari in Haskell, è necessario importare il modulo `Text.Regex.Posix` e utilizzare la funzione `matchRegex` o `matchRegexAll`. Ad esempio, se vogliamo trovare tutte le parole che terminano con "ing" in una stringa, possiamo utilizzare il seguente codice:

```Haskell
import Text.Regex.Posix

matches = matchRegexAll (mkRegex "([a-z]+)ing") "I am coding in Haskell"
```

Il risultato sarà una lista di tuple, ogniuna contenente la posizione iniziale e finale della parola trovata. In questo caso, `("coding", 7, 12)` e `("Haskell", 18, 26)`.

# Approfondimento:
Le espressioni regolari sono state introdotte per la prima volta negli anni '50 da Stephen Kleene, matematico e informatico statunitense. Oggi sono supportate da numerosi linguaggi di programmazione, tra cui Haskell, e sono utilizzate in molti settori come il web scraping, il filtraggio dei dati e la validazione dei formati di input.

Un'alternativa alle espressioni regolari è l'utilizzo delle funzioni di manipolazione delle stringhe fornite dal linguaggio di programmazione stesso. Tuttavia, le espressioni regolari offrono una maggiore flessibilità e potenza di ricerca rispetto a queste funzioni.

Per quanto riguarda l'implementazione, le espressioni regolari in Haskell utilizzano il Motore POSIX di espressioni regolari, che è il più usato a livello globale.

# Vedi anche:
- La documentazione ufficiale del modulo `Text.Regex.Posix` di Haskell: https://hackage.haskell.org/package/regex-compat-0.95.1/docs/Text-Regex-Posix.html
- Un tutorial completo sull'utilizzo di espressioni regolari in Haskell: https://www.haskell.org/tutorial/patterns.html#regular-expressions