---
title:                "Interpolazione di una stringa"
html_title:           "Clojure: Interpolazione di una stringa"
simple_title:         "Interpolazione di una stringa"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Elixir: String Interpolation - Semplice, Veloce e Utile

## Cos'è e Perché?

L'interpolazione di stringhe è una caratteristica di Elixir che consente di inserire valori variabili all'interno di una stringa. Questa tecnica è utile perché rende la formattazione di testo e numeri più leggibile e veloce.

## Come fare:

Ecco un modo semplice per interpolare una stringa in Elixir.

```Elixir
nome = "Mario"
IO.puts "Ciao, #{nome}!"
```

Questo stamperà:

```
Ciao, Mario!
```

L'interpolazione di stringhe può essere utilizzata con qualsiasi espressione. 

```Elixir
a = 5
b = 10
IO.puts "La somma di #{a} e #{b} è #{a + b}."
```
 
Questo stamperà:

```
La somma di 5 e 10 è 15.
```

## Approfondimento:

L'interpolazione di stringhe esiste da molto tempo in vari linguaggi di programmazione, come Perl, Ruby e Python. In Elixir, l'interpolazione di stringhe viene fatta alla compilazione, quindi non c'è alcuna penalità di runtime.

Alternativamente, potresti utilizzare la concatenazione di stringhe (`<>`) o utilizzare `String.concat/2` o `String.concat/3`. Tuttavia, queste alternative sono più verbose e potrebbero non essere pratiche per l'uso in stringhe più lunghe o complesse.

Insomma, Elixir implementa l'interpolazione di stringhe in modo efficiente e pulito, rendendola uno strumento potente e conveniente per la manipolazione di stringhe.

## Vedi anche:

Se vuoi approfondire l'argomento di interpolazione di stringhe in Elixir, consulta le seguenti fonti:

1. Documentazione ufficiale Elixir: [String Interpolation](https://hexdocs.pm/elixir/String.html) 
2. Libro: [Programming Elixir >= 1.6](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
4. [Elixir String Interpolation](https://www.educative.io/edpresso/elixir-string-interpolation) - un conciso tutorial da Educative.