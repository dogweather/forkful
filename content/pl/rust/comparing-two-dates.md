---
title:    "Rust: Porównywanie dwóch dat."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest niezbędnym elementem programowania. Często musimy porównywać daty w celu sprawdzenia, czy dany event już się wydarzył, czy też nie. W Rust istnieje wiele sposobów na porównywanie dat, więc warto poznać je wszystkie, aby mieć wybór w zależności od potrzeb.

## Jak to zrobić

W Rust istnieje kilka metod porównywania dat. Jedną z nich jest użycie metody `.cmp()`, która zwraca Enum `std::cmp::Ordering`. Możemy również użyć dostępnych operatorów porównania, takich jak `<`, `>`, `<=`, `>=`, `==`, `!=`. W przykładzie poniżej przyjrzymy się jak porównywać daty za pomocą tych metod:

```Rust
use std::cmp::Ordering;

fn main() {
    let date1 = chrono::NaiveDate::from_ymd(2021, 10, 1);
    let date2 = chrono::NaiveDate::from_ymd(2021, 8, 15);

    // Porównujemy daty za pomocą metody .cmp()
    match date1.cmp(&date2) {
        Ordering::Less => println!("{} jest wcześniejsza od {}", date1, date2),
        Ordering::Greater => println!("{} jest późniejsza od {}", date1, date2),
        Ordering::Equal => println!("{} jest taka sama jak {}", date1, date2)
    }

    // Porównujemy daty za pomocą operatora >
    if date1 > date2 {
        println!("{} jest późniejsza od {}", date1, date2)
    }

    // Porównujemy daty za pomocą operatora ==
    if date1 == date2 {
        println!("{} jest taka sama jak {}", date1, date2)
    }
}
```

Powyższy kod wypisze:

```
2021-10-01 jest późniejsza od 2021-08-15
2021-10-01 jest późniejsza od 2021-08-15
```

## Deep Dive

Warto również zwrócić uwagę na fakt, że w Rust porównywanie dat odbywa się w oparciu o kalendarz gregoriański. To ważne, ponieważ w niektórych kalendarzach różnica między dwoma datami może wynosić mniej niż dzień, a w innych nawet kilka dni.

W przypadku porównywania dat i czasów, możemy wykorzystać bibliotekę `chrono`, która jest najpopularniejszym narzędziem do pracy z datami w Rust. Możemy również użyć jej w połączeniu z metodą `Duration` do obliczenia różnicy między dwoma datami lub dodania lub odjęcia czasu do dat. Warto też zwrócić uwagę na obsługę stref czasowych i zamianę dat na różne formaty.

## Zobacz także

- [Dokumentacja biblioteki `chrono`](https://docs.rs/chrono/)
- [Porównywanie i obliczanie czasów w Rust](https://medium.com/@azummo/porównywanie-i-obliczanie-czasów-w-rust-47818b1ec1af)
- [Kalendarze w Rust](https://www.youtube.com/watch?v=g8R-7Q83PiE)