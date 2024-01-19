---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Porównywanie dwóch dat polega na określeniu, która data jest wcześniejsza lub późniejsza, lub czy obie daty są identyczne. Programiści robią to, aby sprawdzić upływ czasu między dwiema datami, zarządzać czasowym porządkowaniem wydarzeń, itp.

## Jak to zrobić:

Oto przykład, jak porównać dwie daty w Ruby:

```Ruby
require 'date'

data1 = Date.new(2021, 6, 20)
data2 = Date.new(2022, 1, 15)

if data1 > data2
  puts 'Data1 jest późniejsza'
elsif data1 < data2
  puts 'Data2 jest późniejsza'
else
  puts 'Obie daty są takie same'
end
```

To wypisze `Data2 jest późniejsza`, bo 15 stycznia 2022 jest późniejsze od 20 czerwca 2021.

## Głębsze Zanurzenie:

Porównywanie dat jest fundamentalnym aspektem wielu systemów zarządzania bazami danych i języków programowania od lat. W historii programowania obsługa i porównywanie dat było trudne, ale współczesne języki jak Ruby ułatwiają te zadania.

Alternatywą do użycia klasy `Date` jest użycie klasy `Time`, która zawiera datę oraz czas. Możemy porównać obiekty `Time` w podobny sposób jak obiekty `Date`.

W Ruby, porównywanie dwóch obiektów `Date` jest możliwe dzięki operatorom porównania (>, <, ==, itp). Ruby robi to poprzez porównywanie liczby dni od `Date::ITALY` (daty punktu wyjścia, która jest 2299161_jd) dla obu obiektów `Date`.

## Zobacz też:

1. Dokumentacja Ruby dla klasy Date: https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html
2. Dokumentacja Ruby dla klasy Time: https://ruby-doc.org/core-2.5.0/Time.html
3. Artykuł o obsłudze dat i czasu w Ruby: https://www.rubyguides.com/2015/12/ruby-time-and-date/