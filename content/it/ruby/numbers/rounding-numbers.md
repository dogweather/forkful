---
title:                "Arrotondamento dei numeri"
aliases:
- /it/ruby/rounding-numbers/
date:                  2024-01-26T03:47:38.120938-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arrotondamento dei numeri"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/ruby/rounding-numbers.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Arrotondare i numeri significa aggiustarli al numero intero più vicino o a un grado di precisione specificato. I programmatori arrotondano i numeri per semplificare, per soddisfare le aspettative umane o per adattare i dati a formati specifici—pensate ai calcoli finanziari, alle visualizzazioni grafiche o alla riduzione delle dimensioni di archiviazione.

## Come fare:

```Ruby
# Arrotondamento di base
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Specificare la precisione
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Arrotondamento verso il basso
puts 2.9.floor          # => 2

# Arrotondamento verso l'alto
puts 2.1.ceil           # => 3

# Arrotondamento verso lo zero
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Output di esempio:
```
3
3
3.14
2.68
2
3
-3
-2
```

## Approfondimento
L'arrotondamento dei numeri non è una novità—gli umani lo fanno da secoli per facilitare i calcoli o per lavorare entro i limiti dei loro strumenti. In Ruby, il metodo `round` è versatile, con la capacità di arrotondare al numero intero più vicino per impostazione predefinita o a un punto decimale specificato.

Una alternativa a `round` è `floor` per arrotondare sempre verso il basso, e `ceil` per arrotondare sempre verso l'alto, indipendentemente dal valore del numero. Per semplicemente tagliare i decimali, si può usare `truncate`.

Storicamente, quando si tratta di computer, l'arrotondamento diventa critico nel trattare l'aritmetica a virgola mobile a causa della sua intrinseca imprecisione. Ruby, come la maggior parte dei linguaggi, segue lo standard IEEE 754 per i numeri a virgola mobile, il che significa che gestisce l'arrotondamento in un modo che la maggior parte dei programmatori dovrebbe essere in grado di prevedere e su cui possono fare affidamento.

C'è di più, però—cose come l'arrotondamento bancario (noto anche come arrotondare alla metà pari) sono concetti che gli sviluppatori Ruby potrebbero dover implementare manualmente, poiché il metodo `round` non lo offre direttamente.

## Vedere anche
- La [Documentazione Ruby](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) per il metodo `round` dei Float.
- [Standard IEEE per l'aritmetica a virgola mobile (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Comprendere la precisione in virgola mobile](https://floating-point-gui.de/), per un'insight più profondo su come i computer gestiscono i numeri decimali.
