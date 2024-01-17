---
title:                "Interpolazione di una stringa"
html_title:           "Elm: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché? 

Interpolare una stringa è il processo di inserire dinamicamente dei valori all'interno di una stringa. I programmatori spesso lo fanno per creare stringhe personalizzate che cambiano in base ai dati forniti, come ad esempio messaggi di errore o testi di benvenuto.

## Come fare: 
```Elm
let name = "Marco"
let message = "Ciao " ++ name ++ ", benvenuto!"
```
L'output di questo codice sarebbe "Ciao Marco, benvenuto!" perché la variabile "name" viene interpolata all'interno della stringa "message" con il simbolo "++".

## Approfondimento: 
Interpolare le stringhe è un concetto che esiste da molto tempo nel mondo della programmazione. Prima di Elm, molti linguaggi di programmazione utilizzavano l'approccio delle "concatenazioni" per inserire valori all'interno di una stringa. Tuttavia, combinare stringhe con il simbolo "++" è molto più intuitivo e leggibile rispetto alle concatenazioni. Un'alternativa popolare a Elm per interpolare le stringhe è il linguaggio di programmazione JavaScript. L'implementazione di Elm utilizza una libreria di base chiamata "String" che offre una vasta gamma di funzionalità per manipolare le stringhe, incluso il simbolo "++" per la concatenazione.

## Vedi anche: 
- [Elm documentation on String interpolation](https://elm-lang.org/docs/syntax#string-interpolation)
- [JavaScript template literals for string interpolation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)