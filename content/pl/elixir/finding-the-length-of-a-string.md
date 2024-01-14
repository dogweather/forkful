---
title:    "Elixir: Znajdowanie długości ciągu znaków"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Dlaczego

Znalezienie długości ciągu znaków jest podstawową operacją w wielu językach programowania, w tym w Elixirze. W artykule tym dowiesz się, dlaczego znajomość tej operacji jest ważna dla każdego programisty i jak ją wykonać w Elixirze.

## Jak to zrobić

Zacznijmy od najprostszego przykładu. Chcemy obliczyć długość ciągu znaków "Elixir" i wyświetlić ją na ekranie.

```
Elixir.length("Elixir") #=> 6
```

Możemy także obliczyć długość ciągu znaków przechowywanego w zmiennej.

```
my_string = "Hello World"
Elixir.length(my_string) #=> 11
```

W Elixirze, do obliczenia długości ciągu znaków, używa się funkcji `length`. Możemy również użyć operatora `<>` do łączenia wielu ciągów znaków i obliczenia ich łącznej długości.

```
my_string = "Hello"
another_string = "World"
Elixir.length(my_string <> another_string) #=> 10
```

## Głębsza analiza

W Elixirze, długość ciągu znaków jest obliczana w oparciu o liczbę znaków Unicode. Oznacza to, że litery z diakrytykami mają taką samą wartość co zwykłe litery, co może być zaskoczeniem dla osób, które są przyzwyczajone do innych języków programowania.

Kolejną ważną rzeczą jest to, że w Elixirze ciągi znaków są niezmiennicze. Oznacza to, że nie możemy zmienić wartości ciągu znaków w miejscu, tylko musimy stworzyć nowy ciąg znaków.

## Zobacz także

- Dokumentacja Elixir do funkcji `length`: https://hexdocs.pm/elixir/List.html#length/1
- Przewodnik po ciągach znaków w Elixirze: https://www.tutorialspoint.com/elixir/elixir_string.htm
- Wideo na YouTube o obliczaniu długości ciągu znaków: https://youtu.be/0YOL-q-WYnM