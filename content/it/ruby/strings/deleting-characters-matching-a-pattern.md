---
date: 2024-01-20 17:43:24.112558-07:00
description: "Come Fare: Cancellare caratteri si fa da quando esistono le stringhe\
  \ nei linguaggi di programmazione. In Ruby, `.delete` e `.gsub` sono i metodi\u2026"
lastmod: '2024-04-05T21:53:44.679816-06:00'
model: gpt-4-1106-preview
summary: Cancellare caratteri si fa da quando esistono le stringhe nei linguaggi di
  programmazione.
title: Eliminazione di caratteri che corrispondono a un pattern
weight: 5
---

## Come Fare:
```Ruby
# Rimuovere tutti i numeri da una stringa
stringa = "Estate2023!"
senza_numeri = stringa.gsub(/\d+/, '')  # Usa gsub con una regex per i numeri
puts senza_numeri  # Output: Estate!

# Cancellare solo caratteri speciali
car_speciali = "Bella giornata, eh?!"
pulita = car_speciali.gsub(/[^a-zA-Z\s]/, '')  # Mantieni solo lettere e spazi
puts pulita  # Output: Bella giornata eh

# Sostituire più spazi con uno
spazi = "Troppo   spazio!"
semplificato = spazi.squeeze(" ")  # Usare squeeze per ridurre gli spazi
puts semplificato  # Output: Troppo spazio!
```

## Approfondimento:
Cancellare caratteri si fa da quando esistono le stringhe nei linguaggi di programmazione. In Ruby, `.delete` e `.gsub` sono i metodi comunemente usati per questo compito. `.delete` rimuove tutti i caratteri specificati, mentre `.gsub` (che sta per global substitution) può usare espressioni regolari per cancellazioni più complesse o sostituzioni. Per esempio, `.delete('aeiou')` elimina tutte le vocali in una stringa. Alternativamente `.gsub(/[aeiou]/, '')` fa lo stesso. Ruby implementa `gsub` in modo efficiente, permettendo anche cambiamenti in place con `gsub!`.

## Vedi Anche:
- [Ruby's String#gsub Method](https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub)
- [Ruby Regular Expressions](https://ruby-doc.org/core-2.7.0/Regexp.html)
- [Online Ruby Compiler](https://repl.it/languages/ruby) per esperimenti con codice Ruby.
