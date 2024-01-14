---
title:                "Gleam: Ekstrakcja podciągów"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach wiele aplikacji i narzędzi wymaga przetwarzania tekstów w celu wydobycia konkretnych fragmentów informacji. W tym przypadku bardzo przydatnym narzędziem jest ekstrakcja podciągów. W tym artykule dowiesz się, dlaczego jest to ważna umiejętność w programowaniu i jak jej używać w języku Gleam.

## Jak to zrobić

Aby wykonać ekstrakcję podciągów w Gleam, użyjemy funkcji `substr`, która przyjmuje trzy argumenty: ciąg znaków, indeks początkowy i długość. Dzięki temu możemy łatwo wybrać interesujące nas fragmenty tekstu.

Przykładowo, chcemy wydobyć słowo "programowanie" z ciągu znaków "Uczę się programowania w języku Gleam". W tym przypadku użylibyśmy funkcji `substr` z argumentami "Uczę się programowania w języku Gleam", 9 i 13 (początkowy indeks i długość słowa).

Użycie tej funkcji może wyglądać następująco:

```Gleam
substr("Uczę się programowania w języku Gleam", 9, 13)
```

Wynikiem będzie ciąg znaków "programowania", który możemy wykorzystać dalej w naszym kodzie.

## Głębsza analiza

Funkcja `substr` jest przydatna nie tylko do wyodrębniania słów z ciągów znaków, ale także do wycinania konkretnych fragmentów tekstu. Możemy zmienić wartość drugiego argumentu, aby zacząć ekstrakcję od innego miejsca w ciągu, lub zmienić trzeci argument, aby wyciąć dłuższy lub krótszy fragment. Funkcja ta jest również bezpieczna, ponieważ nie wywołuje błędów w przypadku przekroczenia długości ciągu znaków.

Pamiętaj, że w języku Gleam wszystkie ciągi znaków są niemutowalne, co oznacza, że ​​funkcja `substr` nie zmienia oryginalnego ciągu, ale zwraca nowy ciąg zawierający wybrany fragment.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o funkcji `substr` w języku Gleam, sprawdź poniższe linki:

- Dokumentacja funkcji `substr` w języku Gleam: [https://gleam.run/core/string.html#substr](https://gleam.run/core/string.html#substr)
- Przykładowy projekt wykorzystujący ekstrakcję podciągów w Gleam: [https://github.com/username/example-gleam-project](https://github.com/username/example-gleam-project)
- Artykuł o innych przydatnych narzędziach do przetwarzania ciągów znaków w języku Gleam: [https://medium.com/gleam-lang/string-manipulation-in-gleam-xxx](https://medium.com/gleam-lang/string-manipulation-in-gleam-xxx)

Dziękujemy za przeczytanie tego artykułu i mam nadzieję, że dowiedziałeś się czegoś nowego na temat ekstrakcji podciągów w języku Gleam. Do zobaczenia następnym razem!