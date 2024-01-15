---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Python: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastępowanie tekstu jest powszechnym zadaniem podczas programowania. Dzięki tej technice możemy szybko i wygodnie zmienić dużą ilość tekstów w naszym kodzie. Jest to szczególnie przydatne, gdy potrzebujemy dokonać zmiany w wielu miejscach jednocześnie.

## Jak to zrobić

Aby wykonać zastępowanie tekstu w Pythonie, używamy metody `replace()`. Przyjmuje ona dwa argumenty - pierwszy to tekst, który chcemy zamienić, a drugi to tekst, na który chcemy go zamienić. Przykładowo:

```Python
text = "Cześć świecie!"
new_text = text.replace("świecie", "pythonie")
print(new_text)
```

Output:

```
Cześć pythonie!
```

Mogę też użyć tej metody do usuwania tekstu, podając jako drugi argument pusty ciąg znaków:

```Python
text = "Lorem ipsum dolor sit amet"
new_text = text.replace("ipsum ", "") # usuwam "ipsum "
print(new_text)
```

Output:

```
Lorem dolor sit amet
```

Możemy również dokonywać zastępowania wielokrotnie. Metoda `replace()` zwraca nowy łańcuch znaków zawierający zmienione teksty, więc możemy po prostu nadpisać naszą zmienną `text`:

```Python
text = "Ala ma kota i lubi czytać książki"
text = text.replace("Ala", "Adam").replace("kota", "psa").replace("lubi", "nie lubi")
print(text)
```

Output:

```
Adam ma psa i nie czytać książki
```

## Głęboki zanurzenie

Metoda `replace()` działa jedynie na łańcuchach znaków. Jeśli chcemy dokonać zastępowania na innych elementach, takich jak listy czy tuple, musimy użyć pętli i warunków. Na przykład możemy napisać prostą funkcję, która będzie zastępować wszystkie wystąpienia danej liczby w liście:

```Python
numbers = [2, 5, 2, 1, 2] # lista z liczbami
to_replace = 2 # liczba, która ma być zastąpiona
replacement = 0 # liczba, na którą ma być zastąpiona

def replace_numbers(list, to_replace, replacement):
    for index, value in enumerate(list):
        if value == to_replace:
            list[index] = replacement
    return list

print(replace_numbers(numbers, to_replace, replacement))
```

Output:

```
[0, 5, 0, 1, 0]
```

W ten sposób możemy dokonywać zastępowania w dowolnych typach danych, które można indeksować.

## Zobacz także

- [Dokumentacja metody replace()](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Szybkie i wygodne zastępowanie tekstu w Pythonie](https://towardsdatascience.com/easy-and-fast-text-replacement-in-python-3bea19e987be)
- [Obsługa wyjątków przy zastępowaniu tekstu w Pythonie](https://stackoverflow.com/questions/49455653/how-to-replace-values-which-are-not-in-list-with-nan)