---
title:                "Generowanie losowych liczb"
html_title:           "Fish Shell: Generowanie losowych liczb"
simple_title:         "Generowanie losowych liczb"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Dlaczego


Generowanie losowych liczb jest niezbędną umiejętnością do pisania skryptów i programów w językach programowania. Fish Shell zapewnia wygodne metody generowania losowych wartości, które mogą być wykorzystane w różnych kontekstach, jak na przykład tworzenie testowych danych lub losowe wybory.

## Jak to zrobić

Wykorzystaj funkcje Fish Shell, aby wygenerować losowe liczby całkowite lub zmiennoprzecinkowe, w określonym zakresie.

```Fish Shell
# Generowanie losowej liczby całkowitej z zakresu od 1 do 100
set random_number (random --uniform 100 + 1)
echo $random_number
# Wynik: losowa liczba całkowita z zakresu od 1 do 100
```

```Fish Shell
# Generowanie losowej liczby zmiennoprzecinkowej z zakresu od 0 do 1, z dokładnością do 2 miejsc po przecinku
set random_float (random --uniform 0 1 | printf "%.2f")
echo $random_float
# Wynik: losowa liczba zmiennoprzecinkowa z zakresu od 0 do 1 z dokładnością do 2 miejsc po przecinku
```

## Deep Dive

Fish Shell wykorzystuje algorytm Mersenne Twister do generowania losowych liczb. Jest to jeden z najbardziej popularnych i sprawdzonych algorytmów w tym zakresie, który zapewnia wysoką jakość wygenerowanych liczb. Ponadto, Fish Shell oferuje kilka innych metod generacji losowych wartości, takich jak generowanie liczb losowych z rozkładu normalnego czy losowanie elementów z listy.

## Zobacz też

- Dokumentacja Fish Shell: https://fishshell.com/docs/current/cmds/random.html
- Mersenne Twister: https://pl.wikipedia.org/wiki/Generator_Mersenne_Twister