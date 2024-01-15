---
title:                "Łączenie ciągów znaków"
html_title:           "Python: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli masz wiele napisów w swoim programie i chcesz je połączyć w jeden dłuższy napis, to zapewne zastanawiasz się, jak to najlepiej zrobić. W Pythonie możesz to zrobić za pomocą konkatenacji stringów, czyli po prostu łączenia ich w jeden. Jest to przydatna umiejętność, którą warto poznać.

## Jak to zrobić

Aby połączyć stringi w Pythonie, użyj operatora "+" lub metody ".join()". Oto przykładowy kod:

```Python
# Użycie operatora "+" dla dwóch napisów
string1 = "Dzień"
string2 = "dobry"
new_string = string1 + " " + string2
print(new_string)
# Output: "Dzień dobry"

# Użycie metody ".join()" dla listy stringów
words = ["To", "jest", "przykładowy", "string"]
new_string = " ".join(words)
print(new_string)
# Output: "To jest przykładowy string"
```

## Dogłębna analiza

W Pythonie stringi są niemodyfikowalnymi obiektami, co oznacza, że nie można zmieniać ich już istniejącej wartości. Dlatego też konkatenacja stringów nie modyfikuje już istniejących napisów, a jedynie tworzy nowy. Operacja ta jest wykonywana szybko i jest wydajna, ponieważ w Pythonie stringi są traktowane jako tablice znaków, a więc konkatenacja sprowadza się do po prostu wstawienia jednej tablicy na koniec drugiej.

Więcej informacji na temat konkatenacji stringów w Pythonie można znaleźć w [oficjalnej dokumentacji języka](https://docs.python.org/3/tutorial/introduction.html#strings) oraz na [stronie tutorialu w języku polskim](https://www.python.org.pl/forum/t/2455-Krotki-i-stringi).

## Zobacz także

- [Dokumentacja języka Python](https://docs.python.org/3/)
- [Tutorial języka Python w języku polskim](https://www.python.org.pl/tutorial/)