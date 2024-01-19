---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?

La concatenazione delle stringhe è l'operazione di unire due o più stringhe in una sola. I programmatori la usano per creare output più dinamico e personalizzato.

## Come fare:

La concatenazione delle stringhe in Swift è piuttosto semplice. Puoi usare l'operatore `+` o il metodo `append(_:)`. Ecco alcuni esempi:

```Swift
var saluto = "Ciao, "
let nome = "Marco"
saluto += nome  // "Ciao, Marco"
```
È possibile anche usare l'interpolazione delle stringhe:
```Swift
let cibo = "pizza"
let messaggio = "Adoro la \(cibo)"  // "Adoro la pizza"
```

## Approfondimento

Historicamente, Swift ha migliorato moltissimo la manipolazione delle stringhe rispetto a linguaggi come C e Objective-C. La concatenazione delle stringhe è efficiente ed è in genere preferibile rispetto all'uso di array di stringhe seguito da `join()`, a meno che non si abbiano moltissime stringhe.

Esistono alternative alla concatenazione di stringhe, come l'uso di array di stringhe o l'interpolazione di stringhe. Tali alternative possono essere più efficaci in certi contesti, come quando si hanno molte stringhe piccole da unire.

L'implementazione della concatenazione delle stringhe in Swift garantisce che l'operatore `+` e il metodo `append(_:)` funzionino in tempo lineare, il che significa che il tempo richiesto per eseguire l'operazione scala linearmente con la lunghezza delle stringhe. 

## Approfondisci

Per maggiori informazioni e dettagli su come usare le stringhe in Swift, consulta la [guida ufficiale di Swift sulle Stringhe e Caratteri](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html). 

Se sei interessato a capire meglio le prestazioni delle stringhe in Swift, [questo post di Mike Ash](https://www.mikeash.com/pyblog/friday-qa-2015-11-06-why-is-swifts-string-api-so-hard.html) è un ottimo punto di partenza.