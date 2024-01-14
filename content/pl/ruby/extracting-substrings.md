---
title:                "Ruby: Wycinanie podciągów"
simple_title:         "Wycinanie podciągów"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu potrzebujemy wyciągnąć fragment tekstu z większego ciągu znaków. Może to być np. imię i nazwisko użytkownika z adresu email, numer telefonu z wielu tekstu lub fragment hasła z ciągu znaków. W takich przypadkach bardzo przydatną umiejętnością jest umiejętność ekstrakcji podciągów. Dzięki temu możemy wyselekcjonować tylko te informacje, które nas interesują, a omijać resztę. W tym artykule omówimy, jak w łatwy sposób dokonać ekstrakcji podciągów w języku Ruby.

## Jak to zrobić

Korzystając z języka Ruby, możemy użyć metody `slice` lub `substring` do wyciągnięcia fragmentu tekstu. Przyjrzyjmy się przykładowi:

```Ruby
text = "Witaj w świecie programowania Ruby"
puts text.slice(11, 10) #=> "świecie"
puts text.substring(7, 15) #=> "w świecie pro"
```

Metoda `slice` przyjmuje dwa argumenty - pozycję początkową oraz długość fragmentu. Natomiast metoda `substring` przyjmuje pozycję początkową oraz pozycję końcową fragmentu. W obu przypadkach zwracany jest ciąg znaków, który został wybrany.

Możemy także wykorzystać operator `[]` do wyciągnięcia fragmentu tekstu lub wyrażenie regularne, aby dokonać bardziej skomplikowanej ekstrakcji. Przykłady:

```Ruby
name = "Anna Kowalska"
puts name[5..-1] #=> "Kowalska"
phone_number = "555-123-456"
puts phone_number[/\d{3}-\d{3}-\d{3}/] #=> "555-123-456"
```

## Głębsza analiza

Podczas ekstrakcji podciągów warto pamiętać, że indeksowanie w języku Ruby zaczyna się od 0, a nie od 1. Oznacza to, że pierwszy element tekstu oznaczony jest jako `0`, drugi jako `1` itd. Aby to uwzględnić, musimy odpowiednio dostosować argumenty metody `slice` lub `substring`. Przykład:

```Ruby
text = "Cześć"
puts text.slice(0, 2) #=> "Cz"
```

Jeśli chcemy wyciągnąć ostatni element tekstu, możemy wykorzystać indeks ujemny, np. `-1` oznacza ostatni element, `-2` przedostatni itd.

## Zobacz również

Aby dowiedzieć się więcej o metodach `slice` i `substring` oraz innych sposobach na ekstrakcję podciągów w języku Ruby, zobacz poniższe źródła:

- Dokumentacja Ruby: https://ruby-doc.org/core-3.0.2/String.html#method-i-slice
- Ruby Guides: https://www.rubyguides.com/2019/07/ruby-extract-string/
- Blog GoRails: https://gorails.com/blog/substrings-in-ruby

Dzięki znajomości ekstrakcji podciągów będziesz miał większą kontrolę nad swoim kodem i łatwiej opanujesz manipulację tekstem. Koniecznie wypróbuj te metody w swoich projektach!