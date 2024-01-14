---
title:                "Ruby: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś zechciałby obliczyć datę w przyszłości lub przeszłości? Może być wiele powodów, na przykład chcesz zaplanować wydarzenie lub sprawdzić, kiedy skończy się ważność dokumentu.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości w języku Ruby, należy użyć klasy `Date`. Najpierw musisz załadować bibliotekę:

```Ruby
require 'date'
```

Następnie możesz użyć metody `today` dla obiektu `Date`, aby uzyskać aktualną datę:

```Ruby
Date.today # Output: #<Date: 2021-09-01 ((2459462j, 0s, 0n), +0s, 2299161j)>
```

Aby obliczyć datę w przyszłości, użyj metody `+` i podaj ilość dni, jakie chcesz dodać:

```Ruby
Date.today + 7 # Output: #<Date: 2021-09-08 ((2459469j, 0s, 0n), +0s, 2299161j)>
```

Aby obliczyć datę w przeszłości, użyj metody `-` i podaj ilość dni, jakie chcesz odjąć:

```Ruby
Date.today - 14 # Output: #<Date: 2021-08-18 ((2459455j, 0s, 0n), +0s, 2299161j)>
```

## Dogłębna analiza

Klasa `Date` w języku Ruby opiera się na kalendarzu gregoriańskim i obsługuje daty od 1 stycznia 4713 BC do 31 grudnia 9999 AD. Aby uzyskać więcej informacji na temat tej klasy i jej metod, możesz sprawdzić oficjalną dokumentację języka Ruby (https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html).

## Zobacz także

- https://www.cyberciti.biz/faq/how-to-get-current-date-time-in-ruby/
- https://www.rubyguides.com/2015/05/working-with-dates-in-ruby/
- https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html