---
date: 2024-01-26 04:09:47.739381-07:00
description: "Usare un debugger in Ruby fornisce ai programmatori un superpotere:\
  \ mettere in pausa il loro codice, ispezionare le variabili e passare attraverso\
  \ il loro\u2026"
lastmod: '2024-03-13T22:44:44.055712-06:00'
model: gpt-4-0125-preview
summary: "Usare un debugger in Ruby fornisce ai programmatori un superpotere: mettere\
  \ in pausa il loro codice, ispezionare le variabili e passare attraverso il loro\u2026"
title: Utilizzo di un debugger
---

## Come fare:
Ruby viene fornito con un debugger integrato chiamato `byebug`. Prima, includi `byebug` nel tuo Gemfile e esegui `bundle install`. Quindi, piazza `byebug` esattamente dove vuoi che il tuo programma faccia una pausa.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

Eseguendo questo script si fermerà l'esecuzione a `byebug`, e verrai trasportato in una sessione interattiva dove puoi digitare comandi come:

```
step
next
continue
var local
```

Un esempio di output ti presenterà un prompt che appare così:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## Approfondimento:
Molto tempo fa, prima di `byebug`, i Rubyisti usavano `debugger` e `pry`. Quest'ultimo, `pry`, è più di un debugger; è un potente REPL che può anche essere utilizzato per il debug con il punto di interruzione `binding.pry`.

Alternative al `byebug` di Ruby includono `pry-byebug`, che combina `pry` con la funzionalità di `byebug`, e `ruby-debug`, che è un vecchio gem non più attivamente mantenuto.

Quando invochi `byebug`, il debugger sospende l'esecuzione del tuo codice e ti offre uno sguardo sull'esecuzione. Puoi vedere e cambiare variabili, saltare a diversi punti nel codice, e persino eseguire del codice Ruby linea per linea. È un po' come avere capacità di viaggiare nel tempo per il tuo codice Ruby.

## Vedi Anche:
- Repository GitHub di Byebug: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Documentazione di Pry: [https://github.com/pry/pry](https://github.com/pry/pry)
- Una Guida al Debugging delle App Rails: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
