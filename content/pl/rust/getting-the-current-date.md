---
title:    "Rust: Otrzymywanie aktualnej daty"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Rust to jedno z najbardziej dynamicznych i szybko rozwijających się języków programowania. Jego popularność stale rośnie, a narzędzia i biblioteki dostępne dla programistów są nieustannie ulepszane. Jedną z wielu funkcji, które są dostępne dla użytkowników Rust, jest możliwość uzyskiwania aktualnej daty. W tym krótkim artykule opowiemy o tym, dlaczego warto poznać tę funkcję i jak jej używać.

## Jak to zrobić

Aby uzyskać aktualną datę w języku Rust, musimy użyć funkcji `Local::now()` z biblioteki `chrono`. Oto przykład kodu, który pokazuje, jak użyć tej funkcji i wyświetlić datę w formacie miesiąc/dzień/rok:

```Rust
use chrono::{Local, Datelike};

let now = Local::now();
println!("{}", now.format("%m/%d/%Y"));
```

Wynik tego kodu będzie wyglądał mniej więcej tak: `08/23/2021`. Możemy również użyć `format()` i `print!()` z biblioteki standardowej, aby dostosować w sposób jeszcze bardziej szczegółowy sposób wyświetlania daty. Na przykład, jeśli chcemy dodać do wyświetlania również informację o godzinie, skorzystamy z `%H:%M:%S`:

```Rust
use chrono::Local;

let now = Local::now();
println!("{}", now.format("%m/%d/%Y %H:%M:%S"));
```

Wynik będzie teraz zawierał również informację o godzinie, np. `08/23/2021 14:25:50`.

Jeśli chcesz uzyskać datę w formacie ISO 8601, możemy skorzystać z funkcji `to_rfc3339()`:

```Rust
use chrono::Local;

let now = Local::now();
println!("{}", now.to_rfc3339());
```

To spowoduje wyświetlenie daty w formacie `2021-08-23T14:30:00+02:00`.

## Deep Dive

Teraz, gdy już wiesz, jak uzyskać aktualną datę w języku Rust, warto zagłębić się w to, jak działa biblioteka `chrono`. Pomimo swojej prostoty, biblioteka ta posiada wiele funkcji i możliwości dostosowania wyświetlania daty do naszych potrzeb. Możemy na przykład zmieniać strefy czasowe, dodawać lub odejmować określoną ilość czasu, czy też porównywać różne daty.

Dodatkowo, biblioteka `chrono` jest silnie typowana, co oznacza, że jest bezpieczna i wydajna w użyciu. Programista może mieć pewność, że wyświetlana data będzie zawsze poprawna i nie będzie się martwić, że wystąpią błędy związane z datą.

## Zobacz także

- [Dokumentacja biblioteki `chrono` (język polski)](https://docs.rs/chrono/pl/chrono/)
- [Poradnik o uzyskiwaniu i przetwarzaniu dat w języku Rust](https://dev.to/sivaraam/working-with-date-time-in-rust-1c7g)
- [Strona domowa języka Rust (język polski)](https://www.rust-lang.pl/)