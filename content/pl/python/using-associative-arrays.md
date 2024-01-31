---
title:                "Korzystanie z tablic asocjacyjnych"
date:                  2024-01-30T19:12:51.192241-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z tablic asocjacyjnych"
programming_language: "Python"
category:             "Python"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Tablice asocjacyjne, znane w Pythonie jako słowniki, mapują klucze na wartości, co ułatwia pobieranie, modyfikowanie lub śledzenie danych za pomocą unikalnego identyfikatora. Programiści używają ich ze względu na ich efektywność w dostępie do elementów oraz elastyczność w reprezentowaniu skomplikowanych struktur danych.

## Jak to zrobić:

Tworzenie słownika w Pythonie jest proste. Otaczasz pary klucz-wartość nawiasami klamrowymi `{}`, klucze i wartości oddzielone są dwukropkiem:

```Python
# Utwórz tablicę asocjacyjną (słownik)
my_dict = {"name": "Jan", "age": 30, "city": "New York"}
print(my_dict)
```

Wynik:
```
{'name': 'Jan', 'age': 30, 'city': 'New York'}
```

Dostęp do wartości za pomocą jej klucza jest prosty:

```Python
# Dostęp do wartości
print(my_dict["name"])
```

Wynik:
```
Jan
```

Dodawanie lub aktualizowanie elementów wykonuje się poprzez przypisanie wartości do klucza:

```Python
# Dodaj nową parę klucz-wartość
my_dict["email"] = "jan@example.com"
# Zaktualizuj wartość
my_dict["age"] = 31
print(my_dict)
```

Wynik:
```
{'name': 'Jan', 'age': 31, 'city': 'New York', 'email': 'jan@example.com'}
```

Aby iterować przez elementy słownika:

```Python
# Iteracja przez pary klucz-wartość
for key, value in my_dict.items():
    print(f"{key}: {value}")
```

Wynik:
```
name: Jan
age: 31
city: New York
email: jan@example.com
```

## Zagłębienie się

Tablice asocjacyjne w Pythonie, czyli słowniki, zostały wprowadzone, aby zapewnić strukturę danych dla efektywnego dostępu do danych i ich manipulacji. W przeciwieństwie do sekwencji, które są indeksowane za pomocą zakresu liczb, słowniki są indeksowane za pomocą kluczy, które mogą być dowolnym typem niezmienialnym. Ten wybór projektowy sprawia, że słowniki są idealnie przystosowane do szybkich tabel wyszukiwań, gdzie klucze mapują na unikalne wartości.

Historycznie słowniki Pythona były implementowane przy użyciu tablicy haszującej, zapewniając, że średnia złożoność czasowa dla operacji wyszukiwania, wstawiania i usuwania to O(1). W Pythonie 3.6 i nowszych, słowniki utrzymują także kolejność wstawiania elementów, łącząc zalety tablicy haszującej z przewidywalnością kolejności wstawiania, widoczną w uporządkowanych strukturach danych.

Chociaż słowniki są niezwykle wszechstronne, w niektórych specjalnych przypadkach, alternatywy takie jak `collections.defaultdict` lub `collections.OrderedDict` (przed Pythonem 3.7) mogą być preferowane. `defaultdict` jest szczególnie przydatny, gdy potrzebny jest słownik zwracający domyślną wartość dla nieistniejących kluczy, co upraszcza pewne rodzaje logiki warunkowej. Jednak, przy ciągłym ulepszaniu i ewolucji Pythona, wbudowana klasa słownika często pozostaje pierwszym wyborem dla tablic asocjacyjnych ze względu na jej solidność i wygodę, którą oferuje od razu.
