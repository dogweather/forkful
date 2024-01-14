---
title:                "Ruby: Obliczanie daty w przyszłości lub przeszłości."
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach korzystanie z daty jest nieodłączną częścią programowania. Często spotykamy się z potrzebą obliczenia daty przyszłej lub przeszłej w naszych projektach. W tym wpisie dowiesz się, jak to zrobić w języku Ruby.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w języku Ruby, musimy użyć klasy `Date` i jej metody `new`. Przykład kodu:

```
require 'date'

current_date = Date.new(2021, 4, 20)

# obliczamy datę 30 dni w przód
future_date = current_date + 30
puts future_date # wypisze 2021-05-20

# obliczamy datę 60 dni wstecz
past_date = current_date - 60
puts past_date # wypisze 2021-02-19
```

Mamy również możliwość wykorzystania klasy `Time` i jej metody `at` do określenia konkretnej godziny na przyszłą lub przeszłą datę:

```
require 'time'

current_time = Time.now

# obliczamy datę i godzinę 7 dni po aktualnym czasie
future_datetime = current_time + (7 * 24 * 60 * 60)
puts future_datetime # wypisze np. 2021-04-27 10:20:00 +0200
```

W powyższych przykładach użyliśmy wartości liczbowych (np. 30 dni) do obliczenia daty, jednak możemy także wykorzystać metody `days`, `weeks`, `months` czy `years`, aby dokonać obliczeń bardziej precyzyjnie.

## Deep Dive

Podczas obliczania daty w przyszłości lub przeszłości, warto zwrócić uwagę na to, że klasa `Date` i `Time` mają swoje limity. Dla daty, którą wprowadzamy, musi być odpowiednio zdefiniowany dzień, miesiąc i rok. Dlatego też, jeśli próbujemy obliczyć datę na przykład 30 lutego, otrzymamy błąd. 

Ponadto, jeśli chcemy obliczyć datę w przyszłości lub przeszłości, której dzisiaj nie ma (np. 30 września 2021), musimy dodać lub odjąć również rok, aby było to możliwe. 

## Zobacz także

Dowiedz się więcej o klasie `Date` i `Time` w języku Ruby, korzystając z poniższych linków:

- [Dokumentacja Ruby o klasie Date](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Dokumentacja Ruby o klasie Time](https://ruby-doc.org/core-2.7.0/Time.html) 
- [Artykuł o obliczaniu daty w Ruby](https://www.codingexplorer.com/calculate-date-with-ruby-date-time-library/)