---
title:    "Python: Wyszukiwanie i zamiana tekstu"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego

W tym artykule omówimy podstawy wyszukiwania i zmiany tekstu w Pythonie. Jest to niezbędna umiejętność dla każdego programisty, ponieważ często musimy zmieniać określone wartości lub frazy w naszym kodzie lub plikach tekstowych. Poznanie tej funkcji pomoże ci oszczędzić wiele czasu i wysiłku podczas tworzenia projektów w Pythonie.

## Jak to zrobić

Wyszukiwanie i zmiana tekstu w Pythonie jest bardzo proste i wymaga użycia tylko dwóch funkcji: `find()` i `replace()`. Aby wyszukać określony fragment tekstu, możesz użyć funkcji `find()` z podanym argumentem szukanego słowa lub frazy. Na przykład:

```Python
sentence = "Ten artykuł jest napisany w języku Python"

print(sentence.find("Python")) # wyświetli 33, ponieważ słowo Python zaczyna się w 33. indeksie
```

Aby dokonać zmiany, możesz użyć funkcji `replace()` z podanymi argumentami szukanego słowa lub frazy oraz nowym tekstem, który ma go zastąpić. Na przykład:

```Python
sentence = "Ten artykuł jest napisany w języku Python"

new_sentence = sentence.replace("Python", "C++")
print(new_sentence) # wyświetli: Ten artykuł jest napisany w języku C++
```

Możesz również użyć funkcji `replace()` do zmiany wszystkich wystąpień danego słowa lub frazy w tekście. W przykładzie poniżej zastąpimy wszystkie litery "a" w zdaniu literą "o":

```Python
sentence = "Ten artykuł jest napisany w języku Python"

new_sentence = sentence.replace("a", "o")
print(new_sentence) # wyświetli: Ten ortykuł jest nopisony w języku Python
```

## Głębsze zagłębianie się

Funkcje `find()` i `replace()` mają wiele opcji i argumentów, które mogą ułatwić proces wyszukiwania i zmiany w twoim kodzie. Możesz na przykład użyć argumentu `start` w funkcji `find()` aby określić, od którego indeksu zacząć szukanie. Możesz też użyć argumentu `count` w funkcji `replace()` aby określić, ile razy zmienić szukaną wartość. Aby dowiedzieć się więcej o tych funkcjach i ich opcjach, możesz zajrzeć do dokumentacji Pythona.

## Zobacz również

1. Dokumentacja funkcji `find()` w Pythonie: https://docs.python.org/3/library/stdtypes.html#str.find
2. Dokumentacja funkcji `replace()` w Pythonie: https://docs.python.org/3/library/stdtypes.html#str.replace
3. Tutorial edycji tekstu w Pythonie: https://realpython.com/python-search-sort/#text-editing-functions