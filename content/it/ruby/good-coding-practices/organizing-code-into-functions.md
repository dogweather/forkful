---
date: 2024-01-26 01:11:59.313124-07:00
description: 'Come fare: Immagina di scrivere uno script veloce per salutare gli utenti.'
lastmod: '2024-03-13T22:44:44.056655-06:00'
model: gpt-4-1106-preview
summary: Immagina di scrivere uno script veloce per salutare gli utenti.
title: Organizzazione del codice in funzioni
weight: 18
---

## Come fare:
Immagina di scrivere uno script veloce per salutare gli utenti:

```Ruby
def greet(name)
  "Ciao, #{name}!"
end

puts greet("Alice")   # Output: Ciao, Alice!
puts greet("Bob")     # Output: Ciao, Bob!
```

O forse stai calcolando l'area di un cerchio:

```Ruby
def circle_area(radius)
  Math::PI * radius ** 2
end

puts circle_area(5)   # Output: 78.53981633974483
```

Più ordinato e più facile da gestire, vero?

## Approfondimento
Il concetto di funzioni, conosciute anche come metodi in Ruby, non è nuovo - è vecchio quanto la programmazione stessa. Tornando agli anni '50, le subroutine, come erano conosciute, furono introdotte per ridurre la ridondanza.

Alternative? Certo, hai il codice inline, potresti optare per la programmazione orientata agli oggetti con classi e oggetti, o addirittura per quella funzionale con lambda e procs. Ma le funzioni sono il pane e il burro del codice ordinato. Vuoi prestazioni? Le variabili locali nelle funzioni sono veloci e le funzioni possono restituire valori immediatamente con `return`.

Per quanto riguarda l'implementazione, puoi definire una funzione con `def` e terminarla con `end`. Puoi impostare parametri predefiniti, usare operatori splat per le funzioni variadiche e altro ancora. Le funzioni possono essere semplici o complesse quanto desideri.

## Vedi Anche
- [Documentazione dei metodi di Ruby](https://ruby-doc.org/core-2.7.0/Method.html)
- [Impara a Programmare di Chris Pine](https://pine.fm/LearnToProgram/)
- [Practical Object-Oriented Design in Ruby di Sandi Metz](https://www.poodr.com/)
