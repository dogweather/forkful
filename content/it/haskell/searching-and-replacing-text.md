---
title:                "Ricerca e sostituzione di testo"
html_title:           "Haskell: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Sei stanco di sostituire manualmente ogni singola occorrenza di una parola o frase all'interno di un testo? Vuoi automatizzare questo processo per risparmiare tempo e sforzi? In questo articolo scoprirai come utilizzare Haskell per cercare e sostituire testo in modo efficiente.

## Come Fare

Per iniziare, è necessario importare il modulo "Text.Substitute" per accedere alle funzioni di ricerca e sostituzione di Haskell.

```
import Text.Substitute
```

Per eseguire una sostituzione, puoi utilizzare la funzione "sub" fornendo come input la parola o frase da cercare, quella da sostituire e il testo in cui effettuare la sostituzione. Ad esempio:

```
sub "cane" "gatto" "Il mio cane è bianco"
```

Questo produrrà il seguente output:

```
"Il mio gatto è bianco"
```

Per sostituire tutte le occorrenze, puoi utilizzare la funzione "gsub" nello stesso modo.

Se vuoi ignorare la differenza tra maiuscole e minuscole, puoi utilizzare la funzione "csub" per effettuare una sostituzione case-insensitive.

Haskell offre anche la possibilità di utilizzare espressioni regolari per cercare e sostituire testo. Per fare questo, è necessario importare il modulo "Text.Regex.Substitute" e utilizzare la funzione "subRegex". Ad esempio, per sostituire tutte le vocali in una frase:

```
import Text.Regex.Substitute

subRegex "([aeiou])" "*" "Questa è una frase"
```

Questo produrrà il seguente output:

```
"Q*st* * *n* fr*s*"
```

## Approfondimenti

Oltre alle funzioni di base di "Text.Substitute", esistono altre librerie che offrono funzionalità avanzate per la ricerca e la sostituzione del testo in Haskell. Ad esempio, "regexp-tdfa" fornisce una sintassi più semplice per le espressioni regolari.

Inoltre, è possibile utilizzare la funzione "substituteFromList" per effettuare più sostituzioni contemporaneamente fornendo una lista di tuple contenenti la parola da cercare e quella da sostituire.

## Vedi Anche

- [Documentazione ufficiale di "Text.Substitute"](https://hackage.haskell.org/package/text-substitute)
- [Libreria "regexp-tdfa" per Haskell](https://hackage.haskell.org/package/regexp-tdfa)