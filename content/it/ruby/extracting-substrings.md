---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Estrarre sottocorda significa prendere una porzione specificata di una stringa esistente. Questa operazione è frequente nella programmazione per manipolare e analizzare i dati a livello di stringa.

## Come fare:

In Ruby, possiamo estrarre sottocorda utilizzando l'indice o l'operatore di slicing. Qui ci sono alcuni esempi.

```Ruby
str = "Salve, mondo!"

# Uso dell'indice per ottenere una singola lettera.
puts str[7]   # Stampa: m

# Uso dell'operatore di slicing per ottenere una sottostringa.
puts str[0,5]  # Stampa: Salve
```

Basta specificare la posizione iniziale e la lunghezza della sottostringa.

## Approfondimenti:

Estendendo il concetto dal BASIC (un vecchio linguaggio di programmazione), Ruby ha mantenuto la capacità di manipolare le stringhe a un livello molto dettagliato. È possibile anche utilizzare espressioni regolari per estrarre sottostringhe, sebbene questo sia un po' più avanzato.

```Ruby
# Estrarre la prima parola utilizzando un'espressione regolare.
puts str[/\w+/]   # Stampa: Salve 
```

Tutto ciò che riguarda l'estrazione della sottostringa è implementato nel metodo di indice di Ruby. Questo metodo legge infatti i caratteri della stringa come una sequenza di byte, che è la chiave della sua efficienza.

## Vedi Anche:

1. Documentazione Ruby sulle Stringhe: https://ruby-doc.org/core-2.7.0/String.html
2. Tutorial su Ruby Slicing: https://www.rubyguides.com/2018/11/ruby-slice-method/
3. Lezione Dettagliata sulle Espressioni Regolari in Ruby: https://ruby-doc.org/core-2.1.0/Regexp.html