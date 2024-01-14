---
title:                "Rust: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na string jest częstą czynnością w wielu aplikacjach i często jest to wymagane przez różne biblioteki i narzędzia. Aby z powodzeniem wykorzystać Rust w swoich projektach, warto poznać jak wykonać tę operację.

## Jak to zrobić

Aby przekonwertować datę na string w języku Rust, można użyć funkcji `to_string()` z modułu `chrono::DateTime`. Przykładowy kod wyglądałby następująco:

```rust
use chrono::{DateTime, Local, TimeZone};

let now: DateTime<Local> = Local::now();
let date_string = now.to_string();

println!("Aktualna data i godzina: {}", date_string);
```

W powyższym przykładzie, najpierw importujemy moduł `chrono` zawierający funkcje do operacji na czasie. Następnie, używając funkcji `Local::now()`, pobieramy aktualną datę i godzinę z lokalnego systemu. Wreszcie, za pomocą metody `to_string()` konwertujemy tę datę na string. Wynikiem jest string zawierający datę i czas w formacie ISO 8601.

Można również czasami potrzebować bardziej szczegółowej konwersji daty na string, na przykład z wykorzystaniem formatu niestandardowego. W takim przypadku można użyć funkcji `format()` z modułu `chrono::format::strftime`. Poniżej przedstawiony jest przykład kodu, który konwertuje datę na string w formacie "Dzień Tygodnia, Dzień Miesiąca Rok":

```rust
use chrono::{DateTime, Local, TimeZone};
    use chrono::format::strftime::StrftimeItems;

    let now: DateTime<Local> = Local::now();
    let format = StrftimeItems::new("%A, %d %B %Y");
    let date_string = now.format(format).to_string();

    println!("Aktualna data: {}", date_string);
```

Wynikiem tego przykładu jest string zawierający aktualną datę w postaci "Niedziela, 05 września 2021". Można dowolnie manipulować formatem używając specjalnych symboli dostępnych w module `strftime`.

## Wnikliwe spojrzenie

Podczas konwersji daty na string ważne jest, aby zawsze określać strefę czasową, z której pobierana jest data. W przeciwnym razie, może to prowadzić do nieoczekiwanego wyniku, szczególnie w przypadku używania narzędzi i bibliotek operujących na czasie. W języku Rust, standardową metodą dla stref czasowych jest moduł `chrono::TimeZone`, który zapewnia obsługę różnych stref czasowych.

## Zobacz też

- Dokumentacja modułu `chrono` dla języka Rust: https://docs.rs/chrono/latest/chrono/
- Przewodnik konwersji dat w języku Rust: https://users.rust-lang.org/t/how-can-i-convert-a-date-to-a-string/23764
- Oficjalna strona języka Rust: https://www.rust-lang.org/pl