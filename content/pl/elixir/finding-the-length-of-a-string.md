---
title:    "Elixir: Znajdowanie długości ciągu znaków"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeden z najważniejszych aspektów programowania to manipulowanie ciągami znaków. Potrzeba znania długości danego ciągu występuje często w procesie tworzenia aplikacji i funkcji. W tym poście przybliżymy Ci, jak w prosty sposób wyznaczyć długość łańcucha za pomocą Elixir.

## Jak to zrobić

Zacznijmy od podstaw - deklaracji zmiennej, zawierającej nasz ciąg znaków:

```Elixir
string = "Tutaj jest przykładowy ciąg znaków."
```

Teraz możemy użyć funkcji `String.length(string)` aby wyznaczyć długość ciągu. W wyniku otrzymamy liczbę reprezentującą ilość znaków w ciągu:

```Elixir
len = String.length(string)
IO.puts len
```

Output:
```
33
```

Możemy również wyznaczyć długość wybranego fragmentu ciągu, używając syntaktyki indeksów:

```Elixir
specific_length = String.length(string[5..13])
IO.puts specific_length
```

Output:
```
9
```

## Pogłębiona analiza

Elixir posiada funkcję `String.length/1`, która przyjmuje jeden argument - ciąg znaków - i zwraca jego długość. Wewnętrznie, funkcja ta przekształca dany ciąg do formatu "listy znaków" i zlicza ilość elementów. W związku z tym, nie jest to najbardziej wydajne rozwiązanie na dłuższą metę. Dlatego warto wykorzystać funkcję `byte_size/1`, która zwraca ilość bajtów w ciągu, co jest równoznaczne z długością w przypadku kodowania UTF-8.

## Zobacz także

- [Oficjalna dokumentacja Elixir](https://hexdocs.pm/elixir/String.html#length/1)
- [Blog post na temat funkcji `byte_size/1`](https://blog.appsignal.com/2017/01/31/elixir-alphabet-of-benjamin-b.html)