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

## Perché

Se stai scrivendo un'applicazione web o un programma che richiede la concatenazione di stringhe, è importante conoscere i principi di base di questa operazione. Concatenare le stringhe è utile quando si vuole unire più testo in una singola stringa, creando quindi un'unica entità di testo. Questo può essere utile per la creazione di messaggi personalizzati o per la costruzione di URL dinamiche.

## Come

```elm
concatenaStringhe : String -> String -> String
concatenaStringhe stringa1 stringa2 =
    stringa1 ++ stringa2
```

L'esempio di codice sopra mostra come definire una funzione di concatenazione di stringhe in Elm. La funzione prende due parametri di tipo String e li unisce utilizzando l'operatore di concatenazione ++. Per eseguire la funzione, basta richiamarla con due stringhe come argomenti.

```elm
concatenaStringhe "Ciao " "mondo!" --> "Ciao mondo!"
```

Un'altra opzione è utilizzare la funzione `String.append`, che accetta una lista di stringhe da concatenare.

```elm
String.append ["Ciao ", "mondo!", "!"] --> "Ciao mondo!"
```

## Deep Dive

'Concatenare le stringhe' è un termine usato in informatica per indicare l'operazione di unione di più stringhe in una singola stringa più lunga. In Elm, il simbolo `++` è l'operatore di concatenazione utilizzato per unire due stringhe. Questo operatore accetta due argomenti di tipo String e restituisce una nuova stringa che è la combinazione delle due.

Inoltre, Elm ha anche la funzione `String.join`, che permette di concatenare una lista di stringhe utilizzando un delimitatore specificato. Ad esempio, possiamo unire le parole "ciao" e "mondo" con uno spazio come delimitatore utilizzando questa funzione.

```elm
String.join " " ["ciao", "mondo"] --> "ciao mondo"
```

È importante notare che le stringhe in Elm sono immutabili, il che significa che non possono essere modificate una volta create. Ciò significa che ogni volta che si utilizza l'operatore di concatenazione o una funzione di concatenazione, viene creata una nuova stringa invece di modificare quella esistente.

## See Also

- [Documentazione su stringhe in Elm](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Tutorial di base su Elm](https://guide.elm-lang.org/)