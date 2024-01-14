---
title:                "Elm: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Perché
In questo post, esploreremo come capitalizzare una stringa in Elm. Impareremo perché questa operazione può essere utile e come implementarla usando alcuni esempi di codice.

##Come fare
Per capitalizzare una stringa in Elm, possiamo utilizzare la funzione integrata `String.toUpper`. Vediamo un esempio di come utilizzarla nella nostra codifica:

```Elm
stringa = "ciao mondo" 
capitalizzata = String.toUpper stringa

```
L'output che otteniamo sarà:

`"CIAO MONDO"`

Come puoi vedere, la stringa "ciao mondo" è stata trasformata in una stringa completamente maiuscola.

##Approfondimento
Adesso che sappiamo come capitalizzare una stringa in Elm, vediamo alcune delle ragioni per cui questo può essere utile. In alcuni casi, potresti voler formattare una stringa in modo che rispetti determinati standard o convenzioni, come ad esempio una lettera di presentazione o un testo stampato in maiuscolo. In altri casi, potresti dover confrontare due stringhe senza tenere conto della loro forma maiuscola o minuscola. Utilizzare la funzione `String.toUpper` può rendere questi processi più semplici ed efficienti.

##Vedi anche
- Documentazione su `String.toUpper` in Elm: https://package.elm-lang.org/packages/elm/core/latest/String#toUpper 
- Esempi di utilizzo di `String.toUpper`:https://elmprogramming.com/uppercasing-strings-in-elm 
- Ulteriori informazioni su formattare stringhe in Elm:https://guide.elm-lang.org/interop/strings.html