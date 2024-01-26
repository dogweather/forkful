---
title:                "Concatenazione di stringhe"
date:                  2024-01-20T17:35:23.593240-07:00
model:                 gpt-4-1106-preview
simple_title:         "Concatenazione di stringhe"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Concatenare le stringhe significa unirle insieme, fine a fine, per formarne una nuova. I programmatori lo fanno per manipolare il testo, costruire output dinamici, e gestire dati che cambiano.

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
