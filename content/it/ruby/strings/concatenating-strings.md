---
date: 2024-01-20 17:35:23.593240-07:00
description: 'Come Fare: Per concatenare stringhe in Ruby, puoi usare l''operatore
  `+`, il metodo `concat` o l''interpolazione delle stringhe. Ecco come.'
lastmod: '2024-03-13T22:44:44.041529-06:00'
model: gpt-4-1106-preview
summary: Per concatenare stringhe in Ruby, puoi usare l'operatore `+`, il metodo `concat`
  o l'interpolazione delle stringhe.
title: Concatenazione di stringhe
weight: 3
---

## Come Fare:
Per concatenare stringhe in Ruby, puoi usare l'operatore `+`, il metodo `concat` o l'interpolazione delle stringhe. Ecco come:

```ruby
# Usando l'operatore `+`:
saluto = "Ciao" + " " + "mondo!"
puts saluto # => Ciao mondo!

# Usando il metodo `concat`:
nome = "Roma"
cognome = "Capitale"
nome_completo = nome.concat(" ").concat(cognome)
puts nome_completo # => Roma Capitale

# Usando l'interpolazione delle stringhe:
lingua = "italiano"
frase = "Sto imparando #{lingua}!"
puts frase # => Sto imparando italiano!
```

## Approfondimento
In Ruby, concatenare stringhe è semplice e diretto. In passato, è stata usata la concatenazione per costruire interfacce a linea di comando e output in server o app desktop. Oggi, la concatenazione serve in web apps, scripts, e l'analisi di dati.

Alternativamente alla concatenazione, Ruby offre array di stringhe e operazioni di join, che possono essere più idonee per liste di stringhe o per unirle con un separatore specifico.

Per quanto riguarda l'implementazione, la concatenazione crea un nuovo oggetto stringa, quindi unire molte stringhe può impattare le performance. Ecco perché per operazioni pesanti o iterazioni numerose, si preferisce l'uso di metodi come `<<` o `concat`.

## Vedi Anche
- Ruby documentation on Strings: [Ruby-Doc.org](https://ruby-doc.org/core/String.html)
- Ruby Style Guide on Strings: [RuboCop Ruby Style Guide](https://rubystyle.guide/#strings)
- The Well-Grounded Rubyist, un libro che approfondisce l'uso di stringhe e altre strutture di dati di Ruby.
