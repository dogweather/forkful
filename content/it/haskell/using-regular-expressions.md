---
title:                "Haskell: Utilizzare le espressioni regolari"
simple_title:         "Utilizzare le espressioni regolari"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Le espressioni regolari sono uno strumento fondamentale per la manipolazione di testo e il parsing dei dati. Se si lavora con linguaggi di programmazione come Haskell, avere una conoscenza di base delle espressioni regolari può semplificare drasticamente la gestione di stringhe e pattern.

## Come

Per iniziare ad utilizzare le espressioni regolari in Haskell, si deve importare il modulo "Text.Regex.Posix". Questo modulo contiene una serie di funzioni utili per la creazione, la modifica e l'utilizzo di espressioni regolari.

```Haskell
import Text.Regex.Posix
```

Una volta importato il modulo, si possono utilizzare le funzioni "makeRegex" e "match" per creare un'espressione regolare e cercarla in una stringa.

```Haskell
-- Crea un'espressione regolare per trovare gli indirizzi email
let regex = makeRegex "\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\b" :: Regex
-- Cerca l'espressione regolare nella stringa
let result = "Il mio indirizzo email è email@example.com" =~ regex :: Bool
```

Il valore di "result" sarà "True" se l'espressione regolare viene trovata nella stringa, altrimenti sarà "False".

## Deep Dive

Oltre alle funzioni di base come "makeRegex" e "match", il modulo "Text.Regex.Posix" offre anche una serie di altre funzioni utili per lavorare con espressioni regolari. Ad esempio, la funzione "matchAll" permette di trovare tutte le occorrenze dell'espressione regolare in una stringa, mentre le funzioni "subRegex" e "gsubRegex" permettono di sostituire le occorrenze trovate con un'altra stringa.

Un altro modo per utilizzare le espressioni regolari in Haskell è tramite la libreria "pcre-heavy", che offre una sintassi più flessibile e potente rispetto al modulo "Text.Regex.Posix". Con "pcre-heavy" si possono utilizzare le espressioni regolari come argomenti di funzioni anziché utilizzare la sintassi dei simboli di Haskell.

## Vedi anche

- [Haskell Regular Expressions](https://wiki.haskell.org/Regular_expressions)
- [Tutorial di Regex in Haskell](https://www.codewars.com/kata/regex-tutorial-intermediate-low-coherence-haskell)