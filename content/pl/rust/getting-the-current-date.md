---
title:    "Rust: Pobieranie bieżącej daty"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszym świecie, gdzie technologia jest kluczowym elementem codziennego życia, znajomość aktualnej daty i czasu jest niezbędna. Czy to do tworzenia zadań i wydarzeń w kalendarzu, czy też do dokonania płatności online - aktualna data jest potrzebna do prawidłowego funkcjonowania wielu aplikacji. Dlatego poznanie sposobu pobierania aktualnej daty w języku Rust jest ważnym krokiem dla każdego programisty.

## Jak to zrobić

W języku Rust istnieje kilka sposobów na pobranie aktualnej daty. Pierwszym z nich jest użycie biblioteki standardowej - `std::time::SystemTime`. Przykładowy kod wygląda następująco:

```Rust
use std::time::{SystemTime, UNIX_EPOCH};

let now = SystemTime::now();
let seconds = now.duration_since(UNIX_EPOCH).expect("Time went backwards").as_secs();
```

W powyższym kodzie, najpierw importujemy potrzebne nam moduły z biblioteki standardowej. Następnie używamy metody `now()`, aby pobrać aktualny czas. Aby przekonwertować go na liczbę sekund od początku epoki Unix, musimy odjąć czas początkowy od aktualnego czasu. Ostatecznie, wywołujemy metodę `as_secs()` dla wyniku.

Innym sposobem jest użycie biblioteki `chrono`, która oferuje bardziej zaawansowane funkcje do obsługi dat i czasu. Przykładowy kod wygląda następująco:

```Rust
use chrono::{Utc, DateTime};

let now: DateTime<Utc> = Utc::now();
```

W tym przypadku, importujemy potrzebne nam moduły z biblioteki `chrono` i używamy metody `now()` z odpowiednim strefą czasową - tutaj jest to `Utc`. W wyniku otrzymujemy obiekt typu `DateTime`, który może być łatwo przetworzony według potrzeb.

## Deep Dive

Aby lepiej zrozumieć, jak w języku Rust działa pobieranie aktualnej daty, warto przyjrzeć się implementacji biblioteki standardowej `std::time::SystemTime`. Jest to struktura, która przechowuje czas w postaci liczby sekund od początku epoki Unix oraz nanosekund. Metoda `now()` zwraca wartość obecnej liczby sekund od początku epoki, którą następnie można przetworzyć do wybranego formatu.

## Zobacz też

- Dokumentacja biblioteki standardowej języka Rust: https://doc.rust-lang.org/std/time/struct.SystemTime.html
- Dokumentacja biblioteki `chrono`: https://docs.rs/chrono/0.4.19/chrono/
- Przykładowe projekty wykorzystujące pobieranie aktualnej daty w języku Rust: https://github.com/topics/current-date-rust