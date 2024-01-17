---
title:                "Zmiana wielkości liter w ciągu znaków."
html_title:           "Python: Zmiana wielkości liter w ciągu znaków."
simple_title:         "Zmiana wielkości liter w ciągu znaków."
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Czym jest kapitalizacja napisów i dlaczego programiści jej używają?

Kapitalizacja napisów, zwana także dużymi literami, polega na zamianie wszystkich liter w ciągu znaków na wielkie litery. Programiści często stosują tę technikę, ponieważ ułatwia ona wyróżnienie pewnych elementów w kodzie i poprawia czytelność.

Jak to zrobić?

```Python
napis = "przykładowy tekst"
print(napis.upper())
```
Output: PRZYKŁADOWY TEKST

Deep Dive:

Kapitalizacja napisów jest powszechnie stosowanym sposobem formatowania tekstu w programowaniu. Ma zastosowanie głównie w celu wyróżnienia ważnych nazw zmiennych, funkcji lub komentarzy w kodzie. W przeszłości składnia języków programowania była bardziej restrykcyjna, co wymagało używania kapitalizacji w niektórych przypadkach. Jednak obecnie większość języków programowania nie wymaga tego, więc kapitalizacja jest już kwestią preferencji programisty.

Alternatywną metodą kapitalizacji w Pythonie jest wykorzystanie metody title(). Daje ona efekt zmiany pierwszych liter każdego wyrazu na wielkie.

Implementacja kapitalizacji w języku Python jest bardzo prosta - wystarczy użyć metody upper() na zmiennej zawierającej tekst. Można także dokonać kapitalizacji tylko pojedynczego słowa, wykorzystując metodę capitalize().

Zobacz też:

- Dokumentacja Pythona dotycząca kapitalizacji napisów: https://docs.python.org/3/library/stdtypes.html#string-methods
- Poradnik na temat formatowania tekstu w programowaniu: https://www.dataquest.io/blog/essential-python-strings-tips-tricks/
- Omówienie różnic między kapitalizacją a tytułowaniem w Pythonie: https://python-reference.readthedocs.io/en/latest/docs/str/capitalize.html