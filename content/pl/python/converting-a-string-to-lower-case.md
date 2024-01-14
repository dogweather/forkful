---
title:                "Python: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie ciągu znaków na małe litery jest częstym wyzwaniem w programowaniu. Odpowiednie formatowanie tekstu jest niezbędne w celu poprawnego wyświetlania danych i ułatwienia użytkownikowi korzystania z aplikacji. W tym artykule dowiesz się dlaczego warto przeprowadzić taką konwersję oraz jak to zrobić w języku Python.

## Jak to zrobić

Istnieją różne metody konwertowania ciągu znaków na małe litery w Pythonie. Jedną z nich jest użycie wbudowanej funkcji `lower()`, która zwraca kopię ciągu znaków ze wszystkimi literami zapisanymi małymi literami. Przykładowe użycie tej funkcji wyglądałoby następująco:

```Python
text = "PROGRAMOWANIE W PYTHONIE"
print(text.lower())
```
Output: 

`programowanie w pythonie`

Można również skorzystać z metody `casefold()` która jest jeszcze bardziej uniwersalna, ponieważ konwertuje wszystkie znaki o nieznanej wielkości na małe. Przykładowe użycie tej metody wyglądałoby w ten sposób:

```Python
text = "KOTŁY I FASOLESZ"
print(text.casefold())
```

Output: 

`kotły i fasolesz`

## Głębsze zagadnienia

Każda z tych metod może być użyta w różnych przypadkach. Metoda `lower()` jest często wybierana do konwersji tekstu, który ma być wyświetlony na ekranie dla użytkownika. Z kolei `casefold()` jest zalecana do porównywania tekstów w celu znalezienia różnic bez względu na wielkość liter.

Istnieją również inne metody konwertowania ciągu znaków na małe litery, takie jak `swapcase()`, która zamienia wszystkie litery w ciągu odwrotnym nizielobojowe lub `capitalize()`, która zmienia pierwszą literę na dużą. Wybór odpowiedniej metody zależy od konkretnego przypadku i potrzeb użytkownika.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej na temat operacji na ciągach znaków w języku Python, zerknij na poniższe materiały:

- Samouczek Python: [Operacje na ciągach znaków](https://www.python.org/dev/peps/pep-0008/)
- Wbudowane funkcje tekstowe w Pythonie: [Dokumentacja Python](https://docs.python.org/3/library/string.html)
- Inne artykuły na naszym blogu: [Python.pl](https://python.pl/blog/)