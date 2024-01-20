---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co I Dlaczego?

Wyciąganie podciągów to operacja polegająca na wydzielaniu mniejszych ciągów znaków (podciągów) z większego ciągu znaków. Programiści robią to, aby manipulować danymi, analizować tekst lub filtrować informacje.

## Jak to zrobić:

```Python
# Definiowanie ciągu znaków
napis = "Hello world"

# Wyciąganie podciągu znaków
podciąg = napis[0:5]

# Wyświetlanie wyników
print(podciąg)
```

Kiedy uruchomisz ten kod, zobaczysz wynik:

```
Hello
```

## Głębsza wiedza

(1) Historyczny kontekst - Operacje na ciągach znaków są fundamentem programowania od jego początków. Korzystamy z nich w przypadku przetwarzania języka naturalnego, filtrowania danych i wielu innych zadań.

(2) Alternatywy - Mimo iż nasz sposób to najszybszy i najprostszy, istnieją także inne metody jak np. z użyciem funkcji `slice()`, `substring()` czy regularnych wyrażeń. Wybór metody zależy od Twoich wymagań!

(3) Szczegóły implementacji - Python indeksuje znaki w ciągu znaków, zaczynając od 0. Kiedy piszemy `napis[0:5]`, mówimy Pythonowi: "Daj mi podciąg zaczynając od indeksu 0 i kończąc na 5 (bez 5)"

## Zobacz również

- [Dokumentacja Pythona dotycząca ciągów znaków](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Opis metody slice() na geeksforgeeks.org](https://www.geeksforgeeks.org/python-string-slicing/)
- [Podstawy regularnych wyrażeń w Pythonie na realpython.com](https://realpython.com/regex-python/)