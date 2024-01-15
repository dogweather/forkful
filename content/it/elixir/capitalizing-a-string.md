---
title:                "Maiuscolare una stringa"
html_title:           "Elixir: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa significa trasformare la prima lettera di ogni parola in maiuscolo. Questo può essere utile per formattare i titoli o i nomi in modo standardizzato e leggibile.

## Come fare

Per capitalizzare una stringa in Elixir, possiamo utilizzare la funzione `String.capitalize/1` passando la stringa come argomento. Ad esempio:

```elixir
String.capitalize("ciao amici")
# Output: "Ciao amici"
```

Se la stringa è già capitalizzata, la funzione restituirà la stessa stringa. Inoltre, è possibile utilizzare `String.capitalize/2` per specificare una lingua specifica per la formattazione.

## Approfondimento

La funzione `String.capitalize/1` utilizza le regole di capitalizzazione della lingua inglese, cioè trasforma la prima lettera di ogni parola in maiuscolo. Tuttavia, Elixir offre anche altre funzioni per la capitalizzazione che possono essere utili in contesti diversi:

- `String.capitalize_ascii/1` per utilizzare solo caratteri ASCII per la capitalizzazione
- `String.capitalize/2` per specificare una lingua specifica per la formattazione
- `String.capitalize_all/1` per capitalizzare tutte le lettere della stringa
- `String.capitalize_leading/1` per capitalizzare solo la prima lettera della stringa

## Vedi anche

- [Documentazione ufficiale di Elixir su String.capitalize/1](https://hexdocs.pm/elixir/String.html#capitalize/1)
- [Elixir School: Formattazione di stringhe](https://elixirschool.com/it/lessons/basics/string-formatting/)