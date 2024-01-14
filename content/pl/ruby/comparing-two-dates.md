---
title:    "Ruby: Porównywanie dwóch dat"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego
Porównywanie dat jest niezbędne w wielu aplikacjach Ruby. Jeśli tworzysz aplikację, która wymaga sprawdzania daty, ta umiejętność może być bardzo przydatna. Może ona pomóc Ci zaplanować przyszłe wydarzenia, sprawdzić czy dany dzień jest dniem roboczym czy nie, lub po prostu pomóc w utrzymaniu porządku w danych.

## Jak to zrobić
W Ruby, możemy porównać dwie daty za pomocą operatorów porównania, takich jak "<" i ">". 
Przykładowo, chcąc sprawdzić czy dana data jest przed inną datą, możemy użyć następującego kodu:

```Ruby
date_1 = Date.new(2021, 10, 15) #pierwsza data
date_2 = Date.new(2021, 11, 1) #druga data

if date_1 < date_2
  puts "Pierwsza data jest wcześniejsza niż druga data"
end
```

W powyższym kodzie, najpierw deklarujemy dwie daty przy użyciu konstruktora `Date.new()`. Następnie, w warunku if, porównujemy je za pomocą operatora `<`. Jeśli warunek zostanie spełniony, to zostanie wyświetlony odpowiedni komunikat.

Możemy również porównywać daty używając funkcji `.between?`, która sprawdza czy dana data znajduje się pomiędzy podanymi datami. Przykładowo:

```Ruby
date_1 = Date.new(2021, 10, 15) #pierwsza data
date_2 = Date.new(2021, 11, 1) #druga data
date_3 = Date.new(2021, 10, 20) #trzecia data

if date_3.between?(date_1, date_2)
  puts "Trzecia data znajduje się między pierwszą a drugą datą"
end
```

W powyższym kodzie, funkcja `.between?` zostanie wywołana na trzeciej dacie, sprawdzając czy znajduje się ona pomiędzy pierwszą a drugą datą.

## Głębszy wgląd
W Ruby, daty są przechowywane jako obiekty klasy `Date`. Możemy więc używać różnych metod tej klasy do operowania na datach. Przykładowo, możemy sprawdzić czy dany rok jest rokiem przestępnym za pomocą metody `.leap?`:

```Ruby
date = Date.new(2020, 3, 1) #data
if date.leap?
  puts "Rok 2020 jest rokiem przestępnym"
end
```

Możemy również dodać lub odjąć dni, tygodnie, miesiące lub lata od daty za pomocą metod `.+` i `.-`:

```Ruby
date = Date.new(2021, 1, 1) #data
puts date + 7 #dodajemy 7 dni do daty i wyświetlamy ją
puts date - 1 #odejmujemy 1 dzień od daty i wyświetlamy ją
```

## Zobacz również
- https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html - Oficjalna dokumentacja Ruby dla klasy `Date`
- https://www.theodinproject.com/paths/full-stack-ruby-on-rails/courses/ruby-programming/lessons/dates-and-times#comparing-dates - Dodatkowe informacje na temat porównywania dat w Ruby