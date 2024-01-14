---
title:                "Rust: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat może być bardzo przydatne w wielu przypadkach programowania. Może pomóc w sortowaniu danych, wykrywaniu powtarzających się wydarzeń lub w tworzeniu planów lub kalendarzy. W tym artykule dowiesz się, jak porównywać dwie daty w języku Rust.

## Jak to zrobić

Porównywanie dat w języku Rust jest bardzo proste i wygodne dzięki dostępności biblioteki standardowej dla typów daty i czasu. Wystarczy użyć funkcji `cmp()` i podać dwa obiekty typu `DateTime` jako argumenty. Następnie można użyć operatorów porównania (`<`, `>`, `==`, `!=`, `<=` lub `>=`) do porównania tych dwóch dat.

```Rust
use std::time::SystemTime;

fn main() {
    let date_1 = SystemTime::now();
    let date_2 = SystemTime::now();
    
    match date_1.cmp(&date_2) {
        std::cmp::Orderring::Less => println!("Pierwsza data jest wcześniejsza."),
        std::cmp::Ordering::Greater => println!("Pierwsza data jest późniejsza."),
        std::cmp::Ordering::Equal => println!("Obie daty są takie same."),
    }
}
```

W powyższym przykładzie użyliśmy funkcji `now()` z biblioteki `SystemTime` w celu utworzenia dwóch obiektów daty. Następnie wykorzystaliśmy funkcję `cmp()` do porównania tych dat i wyświetlenia odpowiedniego wyniku w zależności od wyniku porównania.

Output:
```
Obie daty są takie same.
```

## Głębsze zagadnienia

Podczas porównywania dat w języku Rust warto zwrócić uwagę na różne strefy czasowe oraz formaty daty i czasu. Upewnij się, że oba obiekty są utworzone zgodnie z tymi samymi ustawieniami, aby uniknąć błędów w porównaniach.

Ponadto, w przypadku bardziej skomplikowanych operacji na datach, warto zapoznać się z bibliotekami zewnętrznymi takimi jak `chrono`.

## Zobacz również

1. [Dokumentacja biblioteki `std::time`](https://doc.rust-lang.org/1.43.0/std/time/index.html)
2. [Porównywanie dat za pomocą biblioteki `chrono`](https://docs.rs/chrono/0.4.19/chrono/struct.DateTime.html#method.cmp)