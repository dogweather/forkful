---
title:                "Cancellazione di caratteri corrispondenti a un modello"
html_title:           "Elm: Cancellazione di caratteri corrispondenti a un modello"
simple_title:         "Cancellazione di caratteri corrispondenti a un modello"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Eliminare i caratteri che corrispondono ad un determinato modello è un'operazione comune nella programmazione. Questo viene fatto per manipolare e trasformare una stringa di testo, eliminando parti indesiderate o che non soddisfano un certo criterio.

## Come fare:
Ecco un esempio di codice in Elm per eliminare tutte le vocali da una stringa di testo:

```Elm
str = "Ciao amici!"
strWithoutVowels = String.filter (\char -> not (List.member(char, ['a', 'e', 'i', 'o', 'u']))) str

-- Output: "C m c!"
```

## Approfondimento:
In passato, l'utilizzo di espressioni regolari era comune per eliminare i caratteri che corrispondono ad un determinato modello. Tuttavia, con l'avvento di nuovi linguaggi di programmazione come Elm, è possibile utilizzare funzioni di ordine superiore come `String.filter` per rendere il codice più leggibile e mantenibile.

Un'alternativa all'uso di `String.filter` è l'utilizzo di una libreria esterna come `elm-regex` che fornisce funzioni più avanzate per la manipolazione di stringhe basate su espressioni regolari.

È importante notare che quando si eliminano caratteri da una stringa, è necessario tenere conto della codifica dei caratteri per evitare problemi di compatibilità tra sistemi operativi.

## Vedi anche:
- [Documentazione Elm su `String.filter`](https://package.elm-lang.org/packages/elm/core/latest/String#filter)
- [Libreria `elm-regex` per la manipolazione di stringhe basate su espressioni regolari](https://package.elm-lang.org/packages/elm/regex/latest/)