---
title:                "Porównywanie dwóch dat"
html_title:           "C++: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Co i Dlaczego? 

Porównywanie dwóch dat to sposób na ustalenie, która data jest wcześniejsza lub późniejsza, albo czy są takie same. Programiści robią to, aby zarządzać i manipulować chronologią eventów i działań w aplikacjach.

## Jak To Zrobić:

W Rust, używamy typu `std::time::SystemTime` do przechowywania punktów w czasie. Sprawdźmy, jak porównać dwie daty:

```Rust
use std::time::SystemTime;

fn main() {
    let date1 = SystemTime::now();
    let date2 = SystemTime::now();
    
    match date1.duration_since(date2) {
        Ok(_) => println!("date1 jest późniejsza"),
        Err(_) => println!("date1 jest wcześniejsza lub taka sama jak date2"),
    }
}
```
Wypisze "date1 jest wcześniejsza lub taka sama jak date2", ponieważ date1 i date2 są inicjalizowana praktycznie w tym samym czasie. 

## Deep Dive:

Porównywanie dat jest istotną operacją, która ma swoje korzenie jeszcze w erze przed-komputerowej. Ta operacja jest używana w różnych kontekstach, od zarządzania bazami danych po tworzenie harmonogramów i planów.

Rust oferuje kilka alternatyw dla `SystemTime`, takich jak biblioteki zewnętrzne, np. `chrono` albo `time`, które umożliwiają więcej operacji na datach.

Główna zaleta `SystemTime` to prostota i wbudowanie w standardową bibliotekę języka. Jednak, nie obsługuje różnych stref czasowych ani formatów dat. Jeżeli jest to wymagane, warto skorzyć z zewnętrznej biblioteki.

## Zobacz Też:

1. [Dokumentacja Rust na SystemTime](https://doc.rust-lang.org/std/time/struct.SystemTime.html)
2. [Dokumentacja na chrono](https://docs.rs/chrono/0.4.0/chrono/)
3. [Dokumentacja na time](https://docs.rs/time/0.3.3/time/)