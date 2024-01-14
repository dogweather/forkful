---
title:    "Rust: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Dlaczego

Kalkulowanie daty w przeszłości lub przyszłości może być bardzo przydatne w wielu różnych sytuacjach. Na przykład, może to pomóc w planowaniu wydarzeń, w tworzeniu interfejsów użytkownika lub w analizowaniu danych. Dzięki użyciu Rusta do obliczania dat, można mieć pewność, że kod będzie bezpieczny i wydajny.

## Jak To Zrobić

Obliczanie daty w przyszłości lub przeszłości jest łatwe w języku Rust dzięki modułowi `chrono`. Najpierw należy dodać tę bibliotekę do projektu, a następnie użyć funkcji `DateTime` do utworzenia daty. Następnie można wykorzystać funkcję `add` lub `subtract`, aby dodać lub odjąć określoną ilość czasu od daty.

```Rust
use chrono::{DateTime, Utc};
use chrono::Duration;

fn main() {
    let date_now = Utc::now();
    let date_in_future = date_now.add(Duration::days(7));
    let date_in_past = date_now.subtract(Duration::hours(12));
    
    println!("Aktualna data: {}", date_now);
    println!("Data za tydzień: {}", date_in_future);
    println!("Data 12 godzin temu: {}", date_in_past);
}
```

To wyświetli następujące wyniki:

```none
Aktualna data: 2021-08-04 15:00:00 UTC
Data za tydzień: 2021-08-11 15:00:00 UTC
Data 12 godzin temu: 2021-08-04 03:00:00 UTC
```

Można również używać różnych jednostek czasu, takich jak minuty, sekundy czy tygodnie, aby dostosować utworzone daty do swoich potrzeb.

## Deep Dive

Chrono oferuje wiele innych funkcji, które ułatwiają obliczanie dat w przeszłości lub przyszłości. Na przykład, można użyć funkcji `with_timezone` w celu dostosowania daty do określonej strefy czasowej. Można także użyć funkcji `format` do wyświetlania daty w wybranym formacie.

Aby uzyskać więcej informacji o tym, jak korzystać z modułu `chrono`, warto zapoznać się z dokumentacją tej biblioteki.

## Zobacz także

- Dokumentacja Chrono: https://docs.rs/chrono/.
- Przewodnik po języku Rust: https://www.rust-lang.org/learn.
- Przykładowe projekty z użyciem Rusta: https://github.com/rust-lang-projects.