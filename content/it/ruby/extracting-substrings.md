---
title:                "Ruby: Estrazione di sottostringhe"
simple_title:         "Estrazione di sottostringhe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché
A volte nella programmazione Ruby, potresti aver bisogno di estrarre porzioni di una stringa per elaborare o manipolare i dati in modo più efficace.

## Come fare
Per estrarre una sottostringa da una stringa in Ruby, puoi utilizzare il metodo `.slice` o la sua forma abbreviata `.[]` con l'indice iniziale e finale della sottostringa desiderata. Ad esempio:

```Ruby
stringa = "Buongiorno a tutti!"

# Estrae la sottostringa "giorno"
stringa.slice(5,5)

# Stessa uscita
stringa[5,5]
```

Se hai bisogno di estrarre una sottostringa con una lunghezza specifica, puoi utilizzare il metodo `.substring` che prende in input l'indice di inizio e la lunghezza della sottostringa desiderata. Ad esempio:

```Ruby
stringa = "Ciao a tutti!"

# Estrae la sottostringa "tutti"
stringa.substring(6,5)
```

Puoi anche utilizzare un espressione regolare per estrarre una sottostringa in base a un pattern. Ad esempio, se vuoi estrarre solo le vocali da una parola:

```Ruby
parola = "ciao"

# Estrae la sottostringa "ia"
parola[1..-2].gsub(/[^aeiou]/,"")
```

## Approfondimento
Se vuoi imparare di più su come estrarre substrati in Ruby, puoi esplorare i metodi `.slice`, `.substring` e le espressioni regolari. Inoltre, puoi anche sperimentare con più metodi di estrazione per trovare il più efficace per la tua specifica situazione.

## Vedi anche
- [Documentazione di Ruby per i metodi di estrazione](https://ruby-doc.org/core-2.7.1/String.html#method-i-slice)
- [Guida all'espressione regolare in Ruby](https://www.rubyguides.com/2015/06/ruby-regular-expressions/)