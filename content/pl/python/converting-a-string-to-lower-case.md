---
title:    "Python: Konwersja ciągu znaków na małe litery"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu jest potrzebne zmniejszanie wielkości liter w ciągu znaków (string). Może to być przydatne przy porównywaniu lub sortowaniu tekstu. 

## Jak to zrobić

Można użyć metody `lower()` aby skonwertować ciąg znaków na małe litery. Należy pamiętać, że ta metoda nie zmienia oryginalnego ciągu i zwraca nowy ciąg z małymi literami.

```Python
text = "Witaj Świecie!"
print(text.lower())
```

Output: witaj świecie!

Jeśli chcemy zmienić oryginalny ciąg, musimy przypisać wynik metody `lower()` do zmiennej.

```Python
text = "Witaj Świecie!"
text = text.lower()
print(text)
```

Output: witaj świecie!

## Głębsza analiza

Podczas konwersji na małe litery, należy pamiętać o różnicach między językiem angielskim a polskim. W języku angielskim, po zmianie na małe litery, litera `i` przed spółgłoskami `j`, `z` i `w` będzie wyświetlana jako `I`, podczas gdy w języku polskim będzie to `i`.

Przykładowo, jeśli skonwertujemy ciąg "AbcZw" na małe litery, wynikiem w języku angielskim będzie "abczw", a w języku polskim "abcżw". Dzieje się tak, ponieważ w języku polskim litera `z` zmienia się na `ż` po przecinku, co jest preferowanym sposobem zapisu w języku polskim.

Z drugiej strony, jeśli użyjemy metody `casefold()` zamiast `lower()`, zostaną uwzględnione szerokości litery i odpowiednio skonwertowany zostanie na małe litery.

```Python
text = "Witaj Świecie!"
print(text.casefold())
```

Output: witaj świecie!

## Zobacz też

- Dokumentacja Pythona: https://docs.python.org/3/library/stdtypes.html#string-methods
- Porównywanie ciągów znaków (string comparison): https://www.programiz.com/python-programming/methods/string/casefold
- Sortowanie ciągów znaków (string sorting): https://www.geeksforgeeks.org/python-sort-list-according-second-element-sublist/