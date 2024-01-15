---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Rust: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem potrzebujemy obliczyć datę w przyszłości lub w przeszłości. Może to być przydatne, gdy chcemy zaplanować wydarzenie lub sprawdzić, które dni tygodnia przypadały na konkretne daty w naszym życiu. W tym artykule pokażemy, jak wykonać takie obliczenia w języku Rust.

## Jak to zrobić

Do obliczania daty w przyszłości lub w przeszłości w języku Rust wykorzystamy bibliotekę `chrono`, która udostępnia funkcje i typy do pracy z datami i czasem. Aby jej użyć, musimy najpierw dodać odpowiednie zależności w pliku `Cargo.toml` naszego projektu:

```Rust
[dependencies]
chrono = "0.4"
```

Następnie, w pliku z kodem, importujemy bibliotekę `chrono`:

```Rust
use chrono::{DateTime, Duration, NaiveDate, Utc};
```

Teraz możemy wykonać obliczenia. Zobaczmy, jak uzyskać datę jutrzejszą (od bieżącego dnia):

```Rust
let dzisiaj = Utc::now().naive_utc().date();
let jutro = dzisiaj + Duration::days(1);
println!("{}", jutro); // wypisze datę jutrzejszą w formacie YYYY-MM-DD
```

Podobnie możemy obliczyć datę w przeszłości, np. sprzed 10 lat:

```Rust
let dzisiaj = Utc::now().naive_utc().date();
let dziesiec_lat_temu = dzisiaj - Duration::days(365 * 10);
println!("{}", dziesiec_lat_temu); // wypisze datę sprzed 10 lat w formacie YYYY-MM-DD
```

## Wnikliwe spojrzenie

W języku Rust obliczanie dat w przyszłości lub w przeszłości jest bardzo proste dzięki bibliotece `chrono`. Biblioteka ta oferuje również wiele innych przydatnych funkcji, takich jak wykonywanie operacji na czasie czy formatowanie dat. Zachęcamy do zapoznania się z jej dokumentacją, aby poznać więcej możliwości.

## Zobacz też

- Dokumentacja biblioteki `chrono`: https://docs.rs/chrono/0.4.11/chrono/index.html
- Przykładowy projekt wykorzystujący `chrono`: https://github.com/dylanaraps/chronobreak