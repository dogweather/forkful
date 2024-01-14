---
title:                "Ruby: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regular expressions są bardzo przydatnym narzędziem w programowaniu. Pozwalają nam na szybkie i efektywne przeszukiwanie i manipulowanie tekstami, co jest niezbędne w wielu projektach. Dlatego warto nauczyć się jak ich używać.

## Jak to zrobić

Do wykorzystania regular expressions w Ruby musimy najpierw zaimportować bibliotekę "re" za pomocą komendy `require 're'`. Zanim zaczniemy używać regular expressions, musimy także przypisać wartość do zmiennej, która będzie przechowywała nasz tekst. 

Przykładowo, jeśli chcemy znaleźć w tekście wszystkie wystąpienia słowa "Ruby", możemy użyć metody `scan` w następujący sposób:

```Ruby
text = "Kurczę, ale lubię programować w Ruby!"
puts text.scan(/Ruby/)
```

Wynikiem działania tego kodu będzie wypisanie słowa "Ruby" z tekstu, ponieważ w kodzie wyrażenie regularne zostało umieszczone w odwrotnych ukośnikach, co oznacza, że szukamy dokładnego dopasowania z tekstem. 

Możemy także wykorzystać wyrażenie regularne do zastąpienia fragmentów tekstu. Na przykład, jeśli chcemy zamienić wszystkie liczby w tekście na słowo "liczba", możemy użyć metody `gsub`:

```Ruby
text = "Witam, dzisiaj jest 4 października."
puts text.gsub(/\d+/, 'liczba')
```

Wynikiem będzie "Witam, dzisiaj jest liczba października.", ponieważ `\d+` oznacza "jedna lub więcej cyfr" i zostaje zastąpiony słowem "liczba". 

## Deep Dive

W wyrażeniach regularnych można wykorzystać wiele różnych symboli i operatorów, które pozwalają nam na bardziej precyzyjne wyszukiwanie tekstu. Na przykład, `.` oznacza "dowolny znak", `[]` pozwala na zdefiniowanie listy dopuszczalnych znaków, a `+` oznacza "jedno lub więcej wystąpień poprzedniego znaku". 

Istnieją także specjalne znaki, takie jak `\w` (dowolne litery, cyfry i znak podkreślenia), `\s` (białe znaki), oraz `\d` (cyfry). Możliwe jest także wykorzystanie operatorów logicznych, takich jak `|` (alternatywa) czy `^` (negacja). 

Dzięki znajomości tych symboli i operatorów, możemy tworzyć wyrażenia regularne, które dokładnie odpowiadają naszym potrzebom i przyspieszą naszą pracę.

## Zobacz także

- [Dokumentacja Ruby o regular expressions](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Interaktywny kurs wyrażeń regularnych](https://regexone.com/)
- [Artykuł o wyrażeniach regularnych na blogu ThoughtBot](https://thoughtbot.com/blog/going-steady-with-regular-expressions)