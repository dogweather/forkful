---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyszukiwanie i zamienianie tekstu to powszechnie używane techniki programistyczne. Pozwalają na automatyczne modyfikowanie pewnych ciągów znaków w dowolnym tekście, co znacznie usprawnia proces edycji, analizy danych, czy ekstrakcji informacji.

## Jak to zrobić:

Python oferuje wiele praktycznych metod do manipulowania tekstem, takie jak `replace()`. Wypróbujmy to:

```Python
tekst = "Witam na Pythonie."
nowy_tekst = tekst.replace("Pythonie", "programowaniu w Pythonie")
print(nowy_tekst)
```

Wyjście:

```Python
"Witam na programowaniu w Pythonie."
```

`replace()` automatycznie zastępuje wszystkie wystąpienia słowa "Pythonie" słowem "programowaniu w Pythonie".

## Głębsze spojrzenie:

Wyszukiwanie i zamienianie tekstu to techniki służące do manipulowania tekstem, które mają swoje korzenie jeszcze w czasach, kiedy powstawało programowanie.

Alternatywą dla metody `replace()` jest wykorzystanie wyrażeń regularnych z modułu `re`. Dzięki nim możemy wykonać bardziej skomplikowane operacje na tekście, jednak są one trudniejsze do opanowania.

Szczegółem implementacyjnym metody `replace()` jest to, że tworzy ona nowy łańcuch znaków zamiast modyfikować istniejący, co wynika z niezmienności łańcuchów znaków w Pythonie.

## Zobacz też:

Dla pełniejszego zrozumienia tematu, możesz odwiedzić następujące źródła:
1. Dokumentacja Pythona na temat manipulacji tekstami: https://docs.python.org/3/library/stdtypes.html#string-methods
2. Tutorial na temat wyrażeń regularnych: https://docs.python.org/3/howto/regex.html
3. Artykuł NTNU na temat wyszukiwania i zamiany tekstu: http://www.ntnu.no/python/text/text.html