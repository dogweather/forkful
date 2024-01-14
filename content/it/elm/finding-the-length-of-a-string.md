---
title:    "Elm: Lunghezza di una stringa"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché
Trovare la lunghezza di una stringa è fondamentale in Elm per poter manipolare e gestire i dati all'interno di un programma. Conoscere il numero di caratteri presenti in una stringa può essere utile per creare logiche di controllo e per verificare se una stringa rispetta dei requisiti specifici.

## Come fare
Per calcolare la lunghezza di una stringa in Elm, è possibile utilizzare la funzione `String.length`. Questa funzione prende in input una stringa e restituisce un intero che rappresenta il numero di caratteri presenti.

```
📝 Elm code:
stringa = "Ciao, mondo!"
lunghezza = String.length stringa
log "La stringa contiene " ++ (toString lunghezza) ++ " caratteri"
```

```
💻 Output:
La stringa contiene 13 caratteri
```

## Approfondimento
È importante notare che la funzione `String.length` conta il numero di caratteri, non di parole. Ad esempio, se si ha una stringa come "Ciao, questo è un test" la lunghezza sarà di 21 poiché conta anche gli spazi tra le parole. Inoltre, la funzione conta anche i caratteri speciali e di escape come `\n` (newline) e `\t` (tabulazione) che occupano rispettivamente un solo carattere.

## Vedi anche
- [Documentazione ufficiale di Elm](https://elm-lang.org/docs/)
- [Tutorial su come manipolare stringhe in Elm](https://www.elm-tutorial.org/en/06-strings/01-manipulating-strings.html)
- [Esempi di codice per trovare la lunghezza di una stringa in Elm](https://elmprogramming.com/string-length.html)