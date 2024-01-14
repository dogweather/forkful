---
title:    "Ruby: Ekstrakcja podciągów"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego warto wykorzystać wyciąganie podłańcuchów w Ruby?

Wyciąganie podłańcuchów to kluczowa umiejętność w programowaniu Ruby, która pozwala na manipulowanie i przetwarzanie tekstu. Dzięki temu możemy precyzyjnie wybierać fragmenty słów czy znaków, które są nam potrzebne. Jest to szczególnie przydatne w przypadku analizowania i przetwarzania dużych zbiorów tekstu, na przykład w przypadku tworzenia aplikacji internetowych czy analizy danych.

## Jak to zrobić?

Wyciąganie podłańcuchów w Ruby jest bardzo proste i odbywa się przy użyciu kilku funkcji. Aby wyciągnąć podłańcuch, musimy znać jego pozycję lub indeks w ciągu znaków. Następnie możemy wykorzystać funkcję `slice`, `[]` lub `slice!`, aby wyciąć interesujący nas fragment. Dzięki temu możemy wybierać konkretne znaki, wyrazy lub nawet zdania.

Przykładowy kod:
```Ruby
name = "Kasia"
puts name[0] # Wypisze "K"
puts name[1..3] # Wypisze "asi"
puts name.slice(2..-1) # Wypisze "sia"
```

Output:
```
K
asi
sia
```

## Głębszy zanurzenie

Wyciąganie podłańcuchów może być również wykorzystywane do bardziej zaawansowanych zadań, takich jak zastępowanie fragmentów tekstu czy wyodrębnianie liczb lub dat. Możemy także łączyć różne funkcje, aby uzyskać pożądany efekt. Na przykład, możemy wykorzystać funkcję `gsub` do zastąpienia konkretnego fragmentu tekstu innym, a następnie wykorzystać funkcję `split` do podzielenia tekstu na wyrazy.

Przykładowy kod:
```Ruby
text = "Dzisiaj jest piękna pogoda!"
puts text.gsub("piękna", "zła").split(" ").slice(2,3) # Wypisze ["zła", "pogoda!"]
```

Output:
```
zła pogoda!
```

## Zobacz także

- [Dokumentacja Ruby: Wyciąganie podłańcuchów](https://ruby-doc.org/core-2.7.1/String.html#method-i-slice)
- [10 przykładów wyciągania podłańcuchów w Ruby](https://www.rubyguides.com/2015/05/working-with-strings-in-ruby/)
- [Wyciąganie podłańcuchów w praktyce - artykuł na Medium](https://medium.com/@jesse_harold/understanding-string-slicing-in-ruby-5be35d892790)