---
title:                "Ricercare e sostituire testo"
html_title:           "Haskell: Ricercare e sostituire testo"
simple_title:         "Ricercare e sostituire testo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Cos'è & Perché?
La ricerca e sostituzione di testo è un'operazione comune tra i programmatori che consente di trovare determinati elementi all'interno di un testo e sostituirli con altri. Questa operazione è utile per modificare rapidamente e in modo efficiente grandi quantità di codice, risparmiando tempo e fatica.

# Come fare:
Ecco un esempio di come eseguire una ricerca e sostituzione di testo in Haskell:

```Haskell
import Data.Text (replace)
-- Definiamo una funzione per eseguire la ricerca e sostituzione
searchAndReplace :: Text -> Text -> Text -> Text
searchAndReplace old new text = replace old new text

-- Esempio di utilizzo
let oldText = "casa"
let newText = "albero"
let text = "La mia casa è bella."
searchAndReplace oldText newText text
-- Output: "La mia albero è bella."
```

# Approfondimento:
- La ricerca e sostituzione di testo è un concetto che ha origini nel campo dell'editing di testi. Veniva utilizzato per sostituire determinate parole o frasi all'interno di un documento.
- Ci sono anche altri modi per eseguire una ricerca e sostituzione di testo, ad esempio utilizzando espressioni regolari.
- Nell'esempio mostrato sopra, abbiamo utilizzato la funzione "replace" del modulo Data.Text, che è una delle opzioni disponibili in Haskell per eseguire questa operazione.

# Vedi anche:
- [Documentazione di Data.Text](https://hackage.haskell.org/package/text/docs/Data-Text.html)
- [Tutorial di espressioni regolari in Haskell](https://www.schoolofhaskell.com/user/dshevchenko/cookbook/regexpressions)