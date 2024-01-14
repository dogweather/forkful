---
title:                "Rust: Porównywanie dwóch dat"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą, z pewnością spotkałeś już sytuację, w której musiałeś/musiałaś porównać dwa daty. Czy kiedykolwiek zastanawiałeś się, jak w prosty sposób można to zrobić w języku Rust? W tym artykule pokażę Ci kilka przykładów porównywania dat w Rust oraz wyjaśnię, dlaczego może być to przydatne w Twoich projektach.

## Jak to zrobić

Porównywanie dat w Rust jest bardzo proste. Wystarczy użyć wbudowanych bibliotek do pracy z datami oraz kilku funkcji dostępnych w języku. Przedstawiam Ci przykładowy kod:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

// Obecna data
let now = SystemTime::now()
	.duration_since(UNIX_EPOCH)
	.expect("Nieprawidłowy czas");

// Data do porównania
let compare_date = UNIX_EPOCH
	+ Duration::from_secs(1000000);

// Porównanie dat
if now < compare_date {
	println!("Data obecna jest wcześniej niż ta do porównania.");
} else {
	println!("Data obecna jest później niż ta do porównania.");
}

```

W powyższym przykładzie korzystamy z biblioteki `std::time` oraz funkcji `now()` i `expect()` do pobrania bieżącej daty oraz funkcji `from_secs()` do ustawienia daty do porównania. Następnie porównujemy je za pomocą operatora porównania `<` i wyświetlamy odpowiednią informację. 

## Deep Dive

Porównywanie dat może być przydatne w wielu przypadkach, na przykład w systemach rezerwacji, gdzie trzeba sprawdzać dostępność terminów, czy też w aplikacjach finansowych, gdzie trzeba upewnić się, że operacje finansowe są wykonywane w odpowiednim czasie. W języku Rust porównywanie dat jest bardzo wygodne i intuicyjne, co ułatwia pracę z tym elementem w projektach.

## Zobacz również

- [Przewodnik po bibliotekach do pracy z datami w języku Rust](https://doc.rust-lang.org/std/time/index.html)
- [Porównywanie dat w języku Rust na przykładzie projektu](https://blog.rust-lang.org/2015/04/10/Fearless-Concurrency.html)
- [Kurs języka Rust - zaawansowane techniki manipulacji datami](https://www.tutorialspoint.com/rust/rust_date_time.htm)