---
title:                "Estrazione di sottostringhe"
date:                  2024-01-20T17:46:33.980109-07:00
model:                 gpt-4-1106-preview
simple_title:         "Estrazione di sottostringhe"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa & Perché?)
Estrarre sottostringhe significa selezionare parti specifiche di una stringa. Programmatori lo fanno per manipolare testo: validare input, estrarre dati, riformattare output.

## How to: (Come fare:)
In Ruby, possiamo usare diversi metodi per estrarre sottostringhe. Ecco alcuni esempi:

```Ruby
# Utilizzo di parentesi quadre con indice e lunghezza
stringa = "Meravigliosa giornata, programmer!"
sottostringa = stringa[13, 9] # parte dall'indice 13 e prende 9 caratteri
puts sottostringa
# Output: giornata,

# Utilizzo di parentesi quadre con Range
sottostringa_range = stringa[13..21] # include gli indici dal 13 al 21
puts sottostringa_range
# Output: giornata,

# Utilizzo di slice
sottostringa_slice = stringa.slice(13...22) # include gli indici dal 13 al 21
puts sottostringa_slice
# Output: giornata,
```

## Deep Dive (Approfondimento)
Estrarre sottostringhe è un concetto presente fin dagli albori della programmazione. In Ruby, la flessibilità e potenza dei metodi di estrazione riflettono la filosofia del linguaggio di rendere la vita del programmatore più facile.

Oltre ai metodi mostrati sopra, `slice!` modifica la stringa originale rimuovendo la sottostringa, mentre metodi come `[]` e `slice` non la modificano. La scelta tra questi metodi dipende dal caso d'uso.

Implementazione in Ruby è ottimizzata per essere intuitiva e veloce, utilizzando strutture di dati interne efficienti.

## See Also (Vedi Anche)
- La documentazione Ruby su String: [Ruby String Documentation](https://ruby-doc.org/core-2.7.0/String.html)
- "Programming Ruby: The Pragmatic Programmer's Guide": [Programming Ruby](http://ruby-doc.com/docs/ProgrammingRuby/)
- Stack Overflow - spesso ci sono discussioni sull'estrazione di sottostringhe in Ruby: [Stack Overflow](https://stackoverflow.com/questions/tagged/ruby+substring)
