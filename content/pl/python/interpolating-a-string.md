---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Interpolacja napisów w Pythonie polega na wstawianiu wartości zmiennych bezpośrednio do napisów. Programiści korzystają z interpolacji napisów, aby utworzyć dynamiczne napisy, które ułatwiają debugowanie i tworzenie czytelnych wiadomości dla użytkowników.

## Jak to zrobić:

Python oferuje kilka metod na interpolację napisów, ale skupmy się na dwóch najpopularniejszych: f-stringi (od Pythona 3.6) i metodzie `.format()`.

Pierwszy sposób, f-stringi:

```Python
imie = "Jan"
powitanie = f"Cześć, {imie}!"
print(powitanie)
```

W wyniku otrzymujemy:

```
Cześć, Jan!
```

Drugim sposobem jest metoda `.format()`:

```Python
imie = "Jan"
powitanie = "Cześć, {}!".format(imie)
print(powitanie)
```

Wynik jest taki sam:

```
Cześć, Jan!
```

## Pogłębione info:

Interpolacja napisów istnieje od dawna w innych językach programowania, takich jak Perl czy Ruby. W Pythonie interpolacja napisów została wprowadzona w wersji 2.6 za pomocą metody `.format()`. Dopiero w wersji 3.6 dodano f-stringi, które są szybsze i bardziej czytelne.

Istnieją też inne metody, takie jak stary sposób z użyciem operatora `%`, ale jest on rzadko stosowany i nie zalecamy go do nowych projektów. Co więcej, f-stringi oferują wiele zaawansowanych opcji, takich jak możliwość wykonywania wyrażeń w środku placeholderów.

## Zobacz też:

[Official Python Documentation on String Interpolation](https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting)

[Python String Formatting Best Practices](https://realpython.com/python-string-formatting/) 

[The f-Strings in Python](https://realpython.com/python-f-strings/)