---
title:                "Elixir: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa è un'operazione comune quando si lavora con dati di testo. Può essere utile per formattare una stringa in modo da rispettare alcune convenzioni linguistiche o semplicemente per migliorare l'estetica della stringa.

## Come Fare

Capitalize una stringa in Elixir è semplice e può essere fatto in diversi modi. Uno dei modi più comuni è utilizzare la funzione `String.capitalize/1` che accetta una stringa come argomento e restituisce una nuova stringa con la prima lettera maiuscola.

Per esempio:

```Elixir
name = "marco"
String.capitalize(name) # restituisce "Marco"
```

Un'altro modo per fare lo stesso è utilizzare l'operatore concatenazione `<>` per unire la prima lettera maiuscola con il resto della stringa.

```Elixir
name = "marco"
String.first(name) <> String.upcase(String.slice(name, 1..-1)) # restituisce "Marco"
```

Infine, possiamo anche utilizzare la funzione `Enum.reduce/3` per iterare attraverso ogni carattere della stringa e cambiarne il caso.

```Elixir
name = "marco"
Enum.reduce(String.graphemes(name), "", &(&1 <> String.capitalize(&2) <> " ")) |> String.trim() # restituisce "Marco"
```

## Approfondimento

Come già accennato, l'operazione di capitalizzazione è utile per formattare correttamente le stringhe in base alle regole linguistiche. L'encoding di alcuni caratteri può variare da una lingua all'altra e la funzione `String.capitalize/1` tiene conto di queste differenze per garantire una capitalizzazione corretta.

Inoltre, sappiamo che le stringhe in Elixir sono immutabili, quindi quando utilizziamo le funzioni per capitalizzare una stringa, la stringa originale non viene modificata ma ne viene restituita una nuova. Questo è fondamentale per garantire la sicurezza e l'integrità dei dati.

## Vedi Anche

- Documentazione ufficiale di Elixir per la funzione `String.capitalize/1`
- Tutorial su come manipolare le stringhe in Elixir
- Approfondimenti sulle regole linguistiche per la capitalizzazione delle stringhe