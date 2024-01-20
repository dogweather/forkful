---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenazione delle Stringhe in Elm

## Cosa & Perché?
Concatenare le stringhe significa unire due o più stringhe insieme. Lo facciamo per costruire messaggi dinamici.

## Come fare:
In Elm, usiamo la funzione `(++)` per concatenare stringhe. Ecco un esempio:

```
s1 = "Ciao"
s2 = "Mondo"
s = s1 ++ ", " ++ s2
```

Quindi, la stringa `s` conterrà il valore "Ciao, Mondo".

## Approfondimento
La concatenazione delle stringhe è uno degli strumenti di programmazione più antichi. In Elm, usa una tecnica chiamata "chinamento di corde" per l'efficienza. Se preferisci un approccio diverso, potresti usare la funzione `String.concat` o `String.join`, che uniscono le stringhe in una lista.

Per esempio, 

```
s = String.concat ["Ciao", ", ", "Mondo"]
```

Il risultato è "Ciao, Mondo". Questo metodi sono più versatili quando si lavora con molte stringhe.

## Vedi anche
Per saperne di più sulla concatenaione delle stringhe in Elm, consiglio le seguenti fonti:
- [Elm's offical guide](https://guide.elm-lang.org/)
- [Elm's String module documentation](https://package.elm-lang.org/packages/elm/core/latest/String)

Lavora sodo e divertiti con Elm!