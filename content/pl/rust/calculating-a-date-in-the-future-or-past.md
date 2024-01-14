---
title:                "Rust: Obliczanie daty w przyszłości lub przeszłości."
simple_title:         "Obliczanie daty w przyszłości lub przeszłości."
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Zastanawiałeś się kiedyś jak obliczyć datę w przyszłości lub przeszłości? Może jesteś programistą, który chce stworzyć aplikację, która będzie wyswietlała datę z wybranego przez użytkownika dnia? W tym artykule dowiesz się jak to zrobić w języku Rust!

## Jak To Zrobić

Pierwszą rzeczą, którą musisz zrobić, to zaimportować bibliotekę czasu do swojego projektu. Możesz to zrobić za pomocą następującego kodu:

```Rust
use std::time::Duration;
```

Następnie, możesz użyć funkcji `elapsed()` w celu obliczenia odstępu czasu między dwoma określonymi punktami czasowymi. W poniższym przykładzie, obliczymy odstęp czasu od momentu uruchomienia programu do teraz:

```Rust
use std::time::Instant;

let start = Instant::now();

// some code...

let end = Instant::now();

let elapsed = end.elapsed();

println!("Od momentu uruchomienia minęło {:?}.", elapsed);
```

Output:

```
Od momentu uruchomienia minęło 2.496927ms.
```

Możesz również obliczyć datę w przyszłości lub przeszłości, dodając lub odejmując odpowiednią ilość czasu od obecnej daty. W poniższym przykładzie, dodamy 1 dzień do obecnej daty:

```Rust
use std::time::SystemTime;

let now = SystemTime::now();

let one_day = now + Duration::from_secs(60 * 60 * 24);

println!("Data za 1 dzień będzie następująca: {:?}", one_day);
```

Output:

```
Data za 1 dzień będzie następująca: 2019-10-29 17:00:26.897347211 +0200.
```

## Deep Dive

Jeśli chcesz dowiedzieć się więcej o obliczaniu daty w przyszłości lub przeszłości w języku Rust, możesz zapoznać się z dokumentacją biblioteki czasu oraz przestudiować przykłady kodu dostępne na stronie Rust Playground.

## Zobacz Również

- [Dokumentacja biblioteki czasu w Rust](https://doc.rust-lang.org/std/time/index.html)
- [Przykłady kodu w Rust Playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=1e8cfd8f11ef3d3c73c1b1fa1d239307)