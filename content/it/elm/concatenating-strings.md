---
title:                "Concatenazione di stringhe"
html_title:           "Elm: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché è importante?

La concatenazione di stringhe è un'operazione fondamentale nella programmazione, utilizzata per combinare due o più stringhe in una singola stringa. I programmatori spesso utilizzano la concatenazione di stringhe per creare messaggi di output personalizzati o per unire dati forniti da utenti o da altre fonti. In sostanza, la concatenazione di stringhe consente di creare una nuova stringa combinando pezzi di testo esistenti.

## Come fare?

In Elm, la concatenazione di stringhe è semplice e intuitiva. Basta utilizzare l'operatore `++` per unire le stringhe. Ad esempio:

```Elm
"Buongiorno " ++ "a tutti!" -- output: "Buongiorno a tutti!"
```

Puoi anche concatenare più stringhe in una sola espressione:

```Elm
"Benvenuti " ++ "nel " ++ "mondo di " ++ "Elm!" -- output: "Benvenuti nel mondo di Elm!"
```

## Approfondimenti

La concatenazione di stringhe è stata introdotta nel linguaggio di programmazione B alla fine degli anni '50 e da allora è stata adottata da molti altri linguaggi, incluso Elm. Una possibile alternativa alla concatenazione di stringhe è l'utilizzo di una cosiddetta "string builder", un oggetto progettato specificatamente per gestire stringhe che devono essere concatenate frequentemente.

Inoltre, la concatenazione di stringhe in Elm è effettuata in modo efficiente grazie al supporto del compilatore. Piuttosto che creare una nuova stringa ogni volta che viene eseguita l'operazione di concatenazione, il compilatore è in grado di combinare le stringhe senza creare nuovi oggetti stringa ogni volta.

## Vedi anche

- Documentazione ufficiale di Elm sui tipi di dato: https://guide.elm-lang.org/core_language.html#types-and-variables
- Un esempio di utilizzo della concatenazione di stringhe in Elm: https://github.com/elm-tutorial-org/elm-tutorial/tree/master/pizzabo
- Un approfondimento sulla storia della concatenazione di stringhe: https://www.rosettacode.org/wiki/String_concatenation