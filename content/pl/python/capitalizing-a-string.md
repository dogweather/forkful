---
title:                "Python: Zmiana wielkich liter w ciągu znaków"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Dlaczego:Kapitalizacja ciągu znaków jest bardzo przydatną funkcją w programowaniu, gdyż pozwala nam zmienić każde słowo lub wyrażenie na pewien standard, na którym bazujemy w naszym projekcie. Może to być np. format nazw użytkowników lub tytułów artykułów.

Jak to zrobić:

```python
# Przykładowy ciąg znaków
string = "to jest przykładowy ciąg znaków"

# Używając metody capitalize()
capitalized_string = string.capitalize()

# Wyświetlamy wynik
print(capitalized_string)

# Output: To jest przykładowy ciąg znaków
```

Deep Dive:

Kapitalizacja jest procesem zmiany pierwszej litery ciągu znaków na literę wielką. Jednak istnieją przypadki, w których chcielibyśmy kapitalizować więcej niż jedną literę, np. w przypadku nazw własnych. W takim przypadku możemy skorzystać z metody title(), która kapitalizuje pierwszą literę każdego słowa w ciągu. Inną przydatną metodą jest upper(), która zamienia wszystkie litery w ciągu na wielkie litery.

See Also:

- Dokumentacja Pythona dotycząca kapitalizacji ciągu znaków: https://docs.python.org/3/library/stdtypes.html#str.capitalize
- Przydatne funkcje łańcuchowe w Pythonie: https://www.w3schools.com/python/python_strings_methods.asp
- Tutorial na temat kapitalizacji w Pythonie: https://www.programiz.com/python-programming/methods/string/capitalize