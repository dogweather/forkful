---
title:                "Porównywanie dwóch dat"
html_title:           "Ruby: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz z Ruby, z pewnością często spotykasz się z datami. Czasami konieczne jest porównywanie dwóch dat, na przykład do sprawdzenia, czy jakaś operacja została wykonana w określonym przedziale czasowym. W tym artykule dowiesz się, jak porównać dwie daty w Ruby.

## Jak to zrobić

Codziennie dziesiątki tysięcy programistów korzystają z Ruby do tworzenia aplikacji internetowych i mobilnych. Aby porównać dwie daty w Ruby, możesz skorzystać z kilku metod. Pierwszą z nich jest wykorzystanie operatora `>` lub `<`. Przykładowy kod wyglądałby tak:

```Ruby
date_1 = Date.new(2021, 6, 1)
date_2 = Date.new(2021, 7, 1)
if date_1 < date_2
  puts "Date 1 is before date 2."
else
  puts "Date 1 is after date 2."
end
```

W powyższym przykładzie, jeśli data 1 jest wcześniejsza niż data 2, program wyświetli napis "Date 1 is before date 2". W przeciwnym razie wyświetli napis "Date 1 is after date 2".

Kolejną metodą jest użycie metody `compare` z modułu `Comparable`, który jest dostępny w Ruby. Przykładowy kod wyglądałby tak:

```Ruby
date_1 = Date.new(2021, 6, 1)
date_2 = Date.new(2021, 7, 1)
if date_1.compare(date_2) == -1
  puts "Date 1 is before date 2."
elsif date_1.compare(date_2) == 1
  puts "Date 1 is after date 2."
else
  puts "Date 1 is equal to date 2."
end
```

W powyższym przykładzie, metoda `compare` porównuje dwie daty i zwraca wartość `-1` jeśli pierwsza data jest wcześniejsza, `1` jeśli pierwsza data jest późniejsza lub `0` jeśli obie daty są równe.

## Dłuboka Nurkowanie

Jeśli chcesz jeszcze bardziej dokładnie analizować daty, możesz skorzystać z metody `jd` z klasy `Date`. Metoda ta zwraca liczbę Julian Days, czyli liczbowe przedstawienie daty. Przykładowy kod wyglądałby tak:

```Ruby
date_1 = Date.jd(2459373) # 1 lipca 2021
date_2 = Date.jd(2459503) # 31 lipca 2021
if date_1 > date_2
  puts "Date 1 is after date 2."
else
  puts "Date 1 is before date 2."
end
```

W powyższym przykładzie, daty są przekazywane jako liczby Julian Days, a następnie porównywane przez operator `>`, który jest dostępny dla liczb.

## Zobacz też

- Dokumentacja Ruby: https://www.ruby-lang.org/pl/documentation/
- Porównywanie dat w Ruby: https://apidock.com/ruby/v2_5_5/Date/%3C=%3E