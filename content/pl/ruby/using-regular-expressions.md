---
title:                "Ruby: Używanie wyrażeń regularnych"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego korzystać z wyrażeń regularnych?

Wyrażenia regularne są niezwykle przydatnym i potężnym narzędziem w programowaniu w Ruby. Służą one do znajdowania i manipulowania tekstem, co może znacznie ułatwić pracę z danymi w aplikacjach. W tym artykule przekażemy Ci podstawową wiedzę na temat korzystania z wyrażeń regularnych w Ruby, abyś mógł z powodzeniem stosować je w swoich projektach.

## Jak to zrobić?

Aby rozpocząć używanie wyrażeń regularnych w Ruby, musimy najpierw zaimportować odpowiedni moduł. W tym celu użyjemy polecenia `require 'regexp'`. Następnie możemy zadeklarować wyrażenie regularne, np. `pattern = /raz/`, które oznacza, że szukamy tekstu, który zawiera słowo "raz".

Teraz możemy wykorzystać różne metody na obiekcie wyrażenia regularnego, aby przeprowadzić operacje na tekście. Na przykład, możemy użyć metody `match` do sprawdzenia, czy w danym tekście znajduje się dopasowanie do wzorca, czyli naszego wyrażenia regularnego. Oto przykład kodu:

```Ruby
require 'regexp'

text = "Jest raz, bywa dwa razy, ale nigdy trzy razy"

pattern = /raz/

puts text.match(pattern)
```

Output: `raz`

Możemy także użyć metody `scan`, aby znaleźć wszystkie dopasowania naszego wyrażenia regularnego w tekście. Oto przykład:

```Ruby
require 'regexp'

text = "Dodaj +2, odejmij -1"

pattern = /\+\d|-1/

puts text.scan(pattern)
```

Output: `+2, -1`

Ten prosty przykład pokazuje, jak wyrażenia regularne mogą być przydatne w prostych operacjach matematycznych.

## Pogłębiona eksploracja

Wyrażenia regularne oferują ogromne możliwości i ich pełen zakres funkcji jest nieco zaawansowany dla początkujących użytkowników. Jeśli chcesz dowiedzieć się więcej na temat korzystania z wyrażeń regularnych w Ruby, polecamy sprawdzić dokumentację oraz samodzielnie eksperymentować. Poniżej przedstawiamy kilka przydatnych linków:

- [Dokumentacja Ruby o wyrażeniach regularnych](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [Interaktywny tutorial wyrażeń regularnych w Ruby](https://rubular.com/)
- [Artykuł "Wyrażenia regularne w Ruby na przykładach"](https://geek.justjoin.it/wyrazenia-regularne-ruby-przyklady/) (po polsku)

## Zobacz także

- [Kurs Ruby na Udemy](https://www.udemy.com/course/the-complete-ruby-programmer-course/?referralCode=4AAC0095AB1D9986A073)
- [Książka "Kurs programowania w języku Ruby"](https://helion.pl/ksiazki/kurs-programowania-w-jezyku-ruby-jayant-sharma,jpruby.htm) (po polsku)
- [Strona internetowa ruby-lang.org](https://www.ruby-lang.org/)