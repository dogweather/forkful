---
title:                "Wyświetlanie informacji debagujących"
html_title:           "Python: Wyświetlanie informacji debagujących"
simple_title:         "Wyświetlanie informacji debagujących"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?
W druku debugowania można zdefiniować jako praktykę wstawiania linii kodu, które wyświetlają informacje o działaniu programu w celu pomocy programistom w znajdowaniu błędów. Programiści często drukują debug output, aby lepiej zrozumieć, co dzieje się w programie i wykryć ewentualne problemy.

## Jak to zrobić:
Funkcja print() jest powszechną metoda drukowania debug output w Pythonie. Przykładowy kod wygląda tak:
```Python
x = 10
print("The value of x is:", x)
```
Output: 
```
The value of x is: 10
```

## Głębsze zagadnienia:
Pierwszy debugger został stworzony przez pioniera nauk komputerowych, Richarda Hamminga, w 1953 roku. Dziś istnieje wiele innych metod drukowania debug output, takich jak logi, breakpoints i debuggers zintegrowane z IDE. Dla programistów Pythona istnieją również moduły takie jak logging i pdb, które mogą być użyte do drukowania debug output.

## Zobacz też:
[https://www.python.org/dev/peps/pep-0575/](https://www.python.org/dev/peps/pep-0575/) - PEP zawierający procedurę dla programistów, którzy chcą wydawać kod

[https://docs.python.org/3/library/sys.html#sys.stderr](https://docs.python.org/3/library/sys.html#sys.stderr) - Oficjalna dokumentacja Pythona dla modułu sys, który może być pomocny w drukowaniu debug output