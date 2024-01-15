---
title:                "Pisanie do standardowego błędu"
html_title:           "Python: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

W codziennym życiu programistów często dochodzi do błędów i nieoczekiwanych sytuacji, które wymagają diagnostyki. W takich przypadkach, z pomocą przychodzi standardowy strumień błędów, czyli standard error. W tym artykule dowiesz się, dlaczego warto pisać do niego w języku Python.

## Jak to zrobić

Istnieją dwa sposoby na wypisywanie do standard error w Pythonie - za pomocą funkcji wbudowanej `print()` z argumentem `file` oraz za pomocą metody `sys.stderr.write()`. Przykładowy kod wykorzystujący oba sposoby może wyglądać następująco:

```Python
import sys

# wypisanie do standard error z użyciem print()
print("Witaj na standard error!", file=sys.stderr)

# wypisanie do standard error z użyciem sys.stderr.write()
sys.stderr.write("Uruchomiono funkcję x z argumentami y i z\n")
```

Powyższy kod wyświetli odpowiednie wiadomości na standardowym strumieniu błędów. Jeśli korzystasz z terminala, widoczność standard error będzie widać w tym samym miejscu co standardowe wyjście.

## Głębsza analiza

Standard error jest jednym z trzech standardowych strumieni danych w języku Python, obok standardowego wejścia i standardowego wyjścia. Jest to strumień błędów i służy do wyświetlania informacji o ewentualnych błędach lub wyjątkach w kodzie.

Korzystanie z funkcji `print()` z argumentem `file` pozwala na wypisanie wiadomości na standard error w sposób podobny do standardowego wyjścia. Jednak korzystanie ze wbudowanej metody `sys.stderr.write()` jest bardziej precyzyjne i pozwala na wypisanie dowolnej treści bez dodatkowego formatowania.

Warto również wiedzieć, że standard error jest połączony z standardowym wyjściem, więc może być wyświetlany w różnych kolorach lub inaczej oznaczany, w zależności od ustawień terminala lub edytora.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o standardowym strumieniu błędów w Pythonie, zapoznaj się z dokumentacją: 

- https://docs.python.org/3/library/sys.html#sys.stderr
- https://docs.python.org/3/tutorial/inputoutput.html#formatted-string-literals