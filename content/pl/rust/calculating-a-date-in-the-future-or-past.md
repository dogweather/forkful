---
title:    "Rust: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie może wydawać się trudne i skomplikowane, ale dzięki językowi Rust oraz jego silnym typom i wydajnemu systemowi typów, można stworzyć niezwykle dokładne i precyzyjne programy. W tym artykule dowiecie się, dlaczego warto poznać możliwości obliczania dat w przyszłości lub przeszłości przy użyciu Rust.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości, możemy wykorzystać bibliotekę `chrono` dostępną w języku Rust. Poniżej znajduje się przykładowy kod, który pokazuje, jak obliczyć datę dziesięć dni od dzisiaj.

```Rust
use chrono::{Local, Duration};

fn main() {
    let today = Local::today(); // pobranie dzisiejszej daty
    let future_date = today + Duration::days(10); // dodanie 10 dni do dzisiejszej daty
    println!("Data za 10 dni: {}", future_date.format("%d/%m/%Y")); // wyświetlenie sformatowanej daty
}
```

Wyjście z powyższego kodu powinno wyglądać następująco:

```
Data za 10 dni: 30/01/2022
```

Możemy również obliczyć datę w przeszłości, zmieniając jedynie operator `+` na `-` oraz używając metody `subtract` zamiast `add`.

## Głębszy zanurk

Biblioteka `chrono` oferuje również mnóstwo innych możliwości obliczania i manipulowania datami w Rust. Na przykład, możemy zmieniać daty na różne strefy czasowe, obliczać różnice między datami, formatować daty zgodnie ze swoimi preferencjami, a nawet generować losowe daty.

Ważne jest, aby pamiętać o ustawianiu domyślnej strefy czasowej `Local` na początku programu, ponieważ w przeciwnym razie biblioteka będzie korzystać z trybu UTC. Można to zrobić za pomocą kodu:

``` Rust
use chrono::{Local, Timelike};

fn main() {
    let local_now = Local::now();
    println!("Aktualna godzina w Twojej strefie czasowej: {}", local_now.hour());
}
```

## Zobacz także

- Dokumentacja biblioteki `chrono` na stronie: https://docs.rs/chrono/
- Poradnik do rustlang na temat obliczania i manipulacji datami: https://www.rust-lang.org/learn/dates