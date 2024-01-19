---
title:                "Capitalizzare una stringa"
html_title:           "Ruby: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Capitalizzare una stringa significa trasformare la prima lettera di ogni parola in maiuscolo. Lo facciamo per migliorare la leggibilità e l'aspetto formale del testo.

## Come fare:

Vediamo come capitalizzare una stringa in Ruby. Utilizzeremo il metodo `capitalize`.

```Ruby
str = "ciao mondo"
str_capitalize = str.capitalize
puts str_capitalize
```

In questo caso, l'output sarà:

```
Ciao mondo
```

Tuttavia, se vogliamo capitalizzare ogni parola nella stringa, possiamo usare il metodo `split` combinato con `map` e `join`.

```Ruby
str = "ciao mondo"
str_titleize = str.split.map(&:capitalize).join(' ')
puts str_titleize
```

L'output sarà:

```
Ciao Mondo
```

## Approfondimenti:

1. *Contesto storico*: Il metodo `capitalize` è presente in Ruby sin dalla sua prima versione.
2. *Alternative*: Un'altra alternative è l'utilizzo del metodo `titleize` di Rails. Però, funziona solo in Rails, non in Ruby puro. 
3. *Dettagli di implementazione*: Il metodo `capitalize` in Ruby funziona solo con le stringhe ASCII. Se la stringa contiene caratteri non ASCII, i risultati possono essere imprevedibili.

## Link Utili:

1. Documentazione ufficiale di Ruby: [String#capitalize](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
2. Documentazione ufficiale di Rails: [String#titleize](https://apidock.com/rails/String/titleize)