---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Ruby: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co & dlaczego? 
Liczenie dat przyszłych lub przeszłych to proces obliczania daty w przyszłości lub w przeszłości na podstawie podanej daty wyjściowej oraz określonego interwału czasu. Programiści często używają tej metody, aby wyliczyć daty ważnych wydarzeń lub ustalić deadline'y w swoich projektach programistycznych.

## Jak to zrobić? 
W Ruby istnieje kilka sposobów na obliczenie daty w przyszłości lub przeszłości. Jednym z nich jest użycie metody `Time#advance`, która pozwala na dodanie lub odjęcie określonej liczby dni, tygodni lub miesięcy od podanej daty. 

```Ruby 
require 'active_support/core_ext/numeric/time'

Time.now.advance(days: 1) # Output: Tue Mar 30 10:04:15 UTC 2021
Time.now.advance(months: -2) # Output: Sun Jan 31 10:04:15 UTC 2021 
```

Możemy również skorzystać z modułu `Date`, który umożliwia obliczenie daty w bardziej precyzyjny sposób. Przykładowo, możemy obliczyć datę z przesunięciem o 5 dni w przód lub 10 dni w tył.

```Ruby
require 'date'

Date.today + 5 # Output: Fri Apr 02 2021
Date.today - 10 # Output: Thu Mar 18 2021
```

## Głębsze spojrzenie 
Historia liczenia dat jest stara jak świat. Już starożytni Egipcjanie korzystali z kalendarza słonecznego, aby znać daty ważnych wydarzeń. W dzisiejszych czasach, mamy wiele alternatywnych sposobów na obliczenie daty, takich jak korzystanie z gotowych bibliotek lub funkcji w różnych językach programowania.

Implementacja metody `Time#advance` w Ruby wykorzystuje moduł Active Support, który dostarcza wiele przydatnych funkcji rozszerzających podstawowe klasy Ruby. Dzięki temu, możliwe jest wygodne manipulowanie datami w naszych projektach.

## Zobacz również 
- [Dokumentacja Ruby - Metoda Time#advance](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html#method-i-advance)
- [Ruby Guides -  Working with Dates and Times](https://www.rubyguides.com/2015/03/ruby-date-time/)
- [Active Support Basics](https://guides.rubyonrails.org/active_support_basics.html)