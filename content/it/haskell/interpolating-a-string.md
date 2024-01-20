---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolazione di Stringhe in Haskell

## Cosa & Perché?
L'interpolazione di stringhe consente di incorporare espressioni all'interno dei bloci di stringhe che vengono convertiti in valori quando il programma viene eseguito. Questo risulta più comodo e leggibile quando si desidera generare stringhe complesse.

## Come fare:
In Haskell, l'interpolazione delle stringhe si ottiene utilizzando la libreria `Text.Printf`. Ecco un esempio di base:

```Haskell
import Text.Printf

main = do
  let nome = "Mario"
  let eta = 25
  printf "Ciao %s, hai %d anni.\n" nome eta
```
Nell'esempio, `%s` e `%d` sono segnaposto. Quando il programma viene eseguito, ottieni:

```Haskell
Ciao Mario, hai 25 anni.
```
## Approfondimento: 
**Contesto storico**: L'interpolazione delle stringhe esiste da molto tempo in vari linguaggi di programmazione come Perl, Python, e PHP. Haskell utilizza un approccio simile attraverso la sua libreria `printf`.

**Alternative**: Un'altra opzione in Haskell è utilizzare le funzioni di concatenazione di stringhe (`++`), ma questo rende il codice più complicato e meno leggibile rispetto all'interpolazione delle stringhe.

**Dettagli di implementazione**: In `Text.Printf`, il segnaposto `%s` viene utilizzato per le stringhe, `%d` per i numeri interi, `%f` per i numeri a virgola mobile, e così via. La funzione `printf` interpreta la stringa e sostituisce i segnaposto con i valori forniti.

## Da Vedere Anche:
1. [Printf in Haskell](http://hackage.haskell.org/package/base-4.12.0.0/docs/Text-Printf.html) - Documentazione ufficiale di Text.Printf in Haskell.
2. [Printf Programming](https://en.wikipedia.org/wiki/Printf_format_string) - Wikipedia entry that explains the details of printf-style string formatting.
3. [Haskell for Beginners](https://www.fpcomplete.com/haskell/tutorial-for-beginners/) - Resources for learning more about programming in Haskell.