---
title:                "Elm: Utilizzare le espressioni regolari"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perché

Le espressioni regolari sono uno strumento potente per la manipolazione e la ricerca di stringhe di testo all'interno di un programma Elm. Usarle può migliorare notevolmente l'efficienza e la versatilità del tuo codice. 

## Come Farlo

Per utilizzare le espressioni regolari in Elm, iniziamo con l'importare il modulo Regex:

```Elm
import Regex 
```

Ora possiamo definire la nostra espressione regolare con la funzione `Regex.regex` e inserire il pattern desiderato tra apici:

```Elm
let regex = Regex.regex "^[A-Z]{3}-[0-9]{3}$"
```

Questa espressione regolare corrisponde a un codice di formato comune, con tre lettere maiuscole seguite da un trattino e tre numeri. Ora possiamo utilizzare questa espressione regolare per cercare corrispondenze all'interno di una stringa di testo utilizzando la funzione `Regex.contains` e passando la nostra espressione regolare e la stringa di testo come argomenti:

```Elm
let hasMatch = Regex.contains regex "ABC-123" 
-- Restituisce True
```

Possiamo anche utilizzare `Regex.find` per trovare la corrispondenza esatta in una stringa e `Regex.replace` per sostituire una corrispondenza con un'altra stringa.

## Approfondimenti

Mentre questo è solo un esempio semplice di utilizzo delle espressioni regolari in Elm, ci sono molte altre funzioni e opzioni disponibili nel modulo Regex. Ad esempio, si possono utilizzare i gruppi di cattura per estrarre parti specifiche di una corrispondenza o utilizzare modificatori come `caseInsensitive` per rendere la ricerca non sensibile alle maiuscole e minuscole.

Per ulteriori informazioni su come utilizzare le espressioni regolari in Elm, ti consiglio di dare un'occhiata alla documentazione ufficiale di Elm e ai seguenti articoli:

- [Funzioni di Regex in Elm](https://www.devato.com/regex-in-elm/)
- [Espressioni Regolari Fondamentali con Elm](https://thoughtbot.com/blog/regular-expressions-fundamentals-with-elm)

## Vedi Anche

- [Documentazione Regex di Elm](https://package.elm-lang.org/packages/elm/regex/latest)
- [Tutorial di Elm su Codecademy](https://www.codecademy.com/learn/learn-elm)