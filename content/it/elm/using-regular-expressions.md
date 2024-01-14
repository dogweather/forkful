---
title:    "Elm: Utilizzando le espressioni regolari"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché
Capita spesso di dover manipolare dei testi all'interno dei nostri programmi. Grazie all'utilizzo delle espressioni regolari (o regular expressions), possiamo eseguire operazioni complesse di ricerca e sostituzione dei caratteri in modo rapido ed efficiente.

## Come fare
Per utilizzare le espressioni regolari in Elm, dobbiamo prima importare il modulo `Regex`. Possiamo poi utilizzare diverse funzioni come `Regex.contains`, `Regex.replace` e `Regex.split` per eseguire operazioni specifiche sui nostri dati di testo.

Ecco un esempio di codice che ricerca una parola all'interno di una stringa e la sostituisce con un'altra:

```Elm
import Regex

testo = "Ciao amici, benvenuti!"

sostituito = Regex.replace (Regex.regex "amici") (\_ -> "colleghi") testo
-- Output: "Ciao colleghi, benvenuti!"
```

## Approfondimento
Le espressioni regolari possono sembrare complicate, ma seguendo alcune regole di sintassi, possiamo creare dei pattern molto precisi per catturare i nostri dati di testo. Ad esempio, possiamo utilizzare il carattere `.` per rappresentare qualsiasi carattere, `*` per indicare un numero qualsiasi di ripetizioni e `+` per indicare una o più ripetizioni.

Possiamo anche utilizzare dei gruppi `()` per catturare parti specifiche del nostro testo e utilizzarle nella sostituzione. Inoltre, possiamo utilizzare le espressioni regolari anche per validare dati di ingresso come indirizzi email o numeri di telefono.

## Vedi anche
- [Documentazione delle espressioni regolari in Elm](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Tutorial su come utilizzare le espressioni regolari in Elm](https://www.paramander.com/blog/producing-a-string-using-regular-expressions-with-elm)
- [Libreria di espressioni regolari avanzate per Elm](https://github.com/jonaljur/elm-regex-practices)