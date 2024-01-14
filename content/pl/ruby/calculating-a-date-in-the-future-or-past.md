---
title:    "Ruby: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Dlaczego
W dzisiejszym artykule dowiesz się, dlaczego czasami potrzebujemy obliczać datę w przyszłości lub przeszłości w naszych programach w języku Ruby.

# Jak to zrobić
Aby obliczyć datę w przyszłości lub przeszłości, możemy skorzystać z klasy `Date` w Ruby. Najpierw musimy wymagać jej użycia w naszym kodzie, używając polecenia `require`:
```Ruby
require 'date'
```
Następnie możemy użyć metody `new` aby stworzyć nowy obiekt `Date` z danymi, które chcemy obliczyć:
```Ruby
# tworzymy obiekt z aktualną datą
today = Date.new

# lub możemy podać konkretną datę jako argumenty w kolejności rok, miesiąc, dzień
some_date = Date.new(2020, 9, 30)
```
Teraz, aby obliczyć datę w przyszłości lub przeszłości, możemy użyć metody `+` lub `-` wraz z liczbą dni, które chcemy dodać lub odjąć od naszego obiektu `Date`:
```Ruby
# obliczamy datę 5 dni w przód
future_date = today + 5

# obliczamy datę 2 tygodnie wstecz
past_date = today - 14
```
Możemy również porównywać daty, używając operatorów logicznych, na przykład:
```Ruby
# sprawdzamy, czy some_date jest poza zakresem od 1 do 10 dni od today
some_date > today + 1 && some_date < today + 10
=> true
```
Aby dowiedzieć się więcej o dostępnych metodach klasy `Date`, można zajrzeć do dokumentacji Ruby lub skorzystać z różnych tutoriali dostępnych online.

# Deep Dive
Aby jeszcze lepiej zrozumieć jak działa obliczanie daty w przyszłości lub przeszłości w Ruby, warto zobaczyć przykład z wykorzystaniem obiektów `DateTime` oraz `Time`. Klasy te dziedziczą z klasy `Date` i wykorzystują dodatkowe informacje, takie jak godzina czy strefa czasowa. Dzięki temu, możemy precyzyjniej obliczać daty w naszych programach.

# Zobacz również
- [Dokumentacja Ruby o klasie Date](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Przewodnik o pracy z datami w Ruby](https://www.rubyguides.com/2015/06/ruby-date-format/)
- [Tutorial o klasie Time w Ruby](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)