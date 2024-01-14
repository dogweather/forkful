---
title:                "Haskell: Ricerca e sostituzione di testo"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Perché

La ricerca e la sostituzione di testo sono uno strumento essenziale per tutti i programmatori, in particolare per coloro che lavorano con linguaggi di programmazione funzionali come Haskell. Questa funzionalità consente di manipolare facilmente grandi quantità di testo in modo rapido ed efficiente.

# Come fare

Per iniziare ad utilizzare la funzione di ricerca e sostituzione del testo in Haskell, è necessario importare il modulo "Text.Regex". Una volta importato, è possibile utilizzare la funzione "subRegex" per ricercare e sostituire uno specifico pattern all'interno di una stringa.

### Esempio di codice:
```Haskell
import Text.Regex

text = "Questo è un esempio di testo per la ricerca e la sostituzione in Haskell."
nuovo_testo = subRegex (mkRegex "Haskell") text "Python"

--Output: "Questo è un esempio di testo per la ricerca e la sostituzione in Python."
```

In questo esempio, la stringa "Haskell" viene cercata all'interno di "text" e viene sostituita con "Python", creando così una nuova stringa "nuovo_testo" con il risultato.

### Esempio di codice con espressione regolare:
```Haskell
import Text.Regex

testo = "Questo è un esempio di testo per la ricerca e la sostituzione in Haskell."
nuovo_testo = subRegex (mkRegex "(.*)Haskell(.*)") testo "\\1Python\\2"

--Output: "Questo è un esempio di testo per la ricerca e la sostituzione in Python."
```

In questo secondo esempio, viene utilizzata un'espressione regolare per cercare uno specifico pattern all'interno della stringa "testo" e sostituirlo con "Python". L'espressione regolare viene utilizzata per catturare qualsiasi testo che si trovi prima e dopo la parola "Haskell" e poi inserirli nella nuova stringa "nuovo_testo" insieme alla parola "Python".

# Approfondimento

In Haskell, la funzione "subRegex" accetta diversi parametri per la ricerca e la sostituzione di testo. Ad esempio, è possibile specificare un parametro "caseSensitive" per indicare se la ricerca deve essere effettuata in modo case-sensitive o meno. Inoltre, è anche possibile utilizzare un'alternativa alla funzione "mkRegex" chiamata "mkRegexWithOpts", che accetta un parametro opzionale "RegexOptions" per specificare le opzioni di ricerca come ad esempio "caseInsensitive" o "multiline".

# Vedi anche

- [Documentazione di Text.Regex](https://hackage.haskell.org/package/regex-tdfa-1.3.1.0/docs/Text-Regex.html)
- [Tutorial su advanced search and replace in Haskell](https://wiki.haskell.org/Regex_as_a_parser)
- [Esempio di utilizzo di regular expressions in Haskell](https://www.youtube.com/watch?v=0SApfIKUd7c)