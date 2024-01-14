---
title:                "Gleam: Porównywanie dwóch dat"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dwóch dat jest powszechnym wyzwaniem podczas programowania i może być trudne dla wielu programistów. Jednak znalezienie skutecznej i prostszej metody może pomóc w rozwiązaniu tego problemu. W tym blogu dowiesz się, jak porównać dwie daty w języku programowania Gleam.

## Jak to zrobić

Porównywanie dwóch dat w Gleam jest proste dzięki wykorzystaniu modułu `DateTime`. Aby porównać dwa daty, wystarczy użyć następującego kodu:

```Gleam
DateTime.compare(first_date, second_date)
```

Wywołanie tej funkcji zwróci jedną z trzech wartości: `DateTime.Equal`, `DateTime.Greater` lub `DateTime.Less`, w zależności od tego czy pierwsza data jest równa, większa lub mniejsza od drugiej.

Aby lepiej zrozumieć sposób działania tej funkcji, przyjrzyjmy się poniższemu przykładowi:

```Gleam
import gleam/datetime.{ compare, Equal, Greater, Less }

fn main() {
  let date_1 = DateTime.new(2021, 1, 1)
  let date_2 = DateTime.new(2022, 1, 1)

  let result = compare(date_1, date_2)

  case result {
    Equal -> println("Obie daty są równe")
    Greater -> println("Pierwsza data jest późniejsza niż druga")
    Less -> println("Pierwsza data jest wcześniejsza niż druga")
  }
}
```

W tym przykładzie `result` będzie miało wartość `Greater`, ponieważ pierwsza data jest późniejsza niż druga. Następnie w zależności od tego, jaka wartość zostanie zwrócona, wyświetlimy odpowiedni komunikat.

## Deep Dive

Jeśli chcesz dowiedzieć się więcej o porównywaniu dat w Gleam, możesz przejrzeć dokumentację modułu `DateTime`. Tam znajdziesz informacje o innych funkcjach, które mogą być przydatne przy porównywaniu dat, np. funkcja `is_after?`, która sprawdza czy pierwsza data jest późniejsza niż druga.

Pamiętaj również, aby dokładnie znać format daty, który jest akceptowany przez funkcje w module `DateTime`. Szczegóły na ten temat również znajdziesz w dokumentacji.

## Zobacz także

- Dokumentacja modułu `DateTime` w języku programowania Gleam: https://gleam.run/modules/date_time.html
- Porównywanie dat w innym języku programowania: https://www.w3schools.com/js/js_dates.asp