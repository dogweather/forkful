---
title:                "Lavorare con i numeri complessi"
aliases:
- /it/ruby/working-with-complex-numbers/
date:                  2024-01-26T04:45:16.769770-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con i numeri complessi"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
I numeri complessi, costituiti da una parte reale e una immaginaria (come 3+4i), sono fondamentali in ingegneria e fisica. I programmatori li utilizzano in simulazioni, elaborazione di segnali e nella risoluzione di equazioni che non funzionano bene solo con i numeri reali.

## Come fare:
Ruby rende molto semplice la gestione dei numeri complessi. Puoi crearli e manipolarli utilizzando la classe Complex:

```ruby
require 'complex'

# Crea numeri complessi
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Operazioni di base
sum = c1 + c2               # => (5.0+9.0i)
difference = c1 - c2        # => (1.0-1.0i)
product = c1 * c2           # => (-14.0+23.0i)
quotient = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# Coniugato, magnitudo e fase
coniugato = c1.conjugate    # => (3.0-4.0i)
magnitudo = c1.abs          # => 5.0
fase = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 radianti

# Metodi specifici per i complessi
polare = c1.polar            # => [5.0, 0.9272952180016122]
rettangolare = c1.rect       # => [3.0, 4.0]
```

## Approfondimento
I numeri complessi non sono una novità: sono presenti sin dal XVI secolo, risolvendo equazioni senza soluzioni reali. A parte la matematica, computazionalmente, la classe Complex di Ruby svolge il lavoro pesante, supportata dal modulo Math per funzioni trigonometriche e trascendentali.

I linguaggi di programmazione precedenti richiedevano una gestione manuale delle parti reale e immaginaria. Alcuni, come Fortran e C++, dedicano librerie speciali all'aritmetica complessa.

L'approccio di Ruby incorpora il supporto per i numeri complessi nella sua sintassi, liberandoti dal reinventare la ruota. Dietro le quinte, la classe Complex si occupa della matematica, mentre Ruby si prende cura delle interazioni tra oggetti.

## Vedi Anche
- Documentazione Ruby su Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- Il punto di vista di MathWorld sui Numeri Complessi: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- Una introduzione visiva ai numeri complessi e al motivo per cui sono utili: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
