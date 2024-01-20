---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pobieranie strony internetowej to proste jak bulka z masłem - oznacza to zapisanie jej kopii na twoim lokalnym systemie. Programiści robią to, aby analizować, przetwarzać struktury na stronie, lub po prostu dla offline-owego używania.

## Jak to zrobić:

```Rust
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("https://www.example.com").await?;

    let content = response.text().await?;

    println!("{}", content);

    Ok(())
}
```

Powinieneś zobaczyć wydruk zawartości strony `https://www.example.com`.

## Deep Dive

Początki pobierania stron internetowych sięgają epoki korzeni HTTP w latach 90-tych. Alternatywą dla Reqwest używanego tutaj jest Hyper, który jest również doskonałym pakietem Rust, ale jest bardziej skomplikowany w czystym użyciu.

Pobieranie strony polega na wysłaniu zapytania GET do serwera docelowego. Twoje zapytanie jest przekazywane od jednego routera do drugiego, aż dotrze do serwera docelowego, który zwraca odpowiedź, którą następnie przeglądasz.

## Zobacz także:

- [Dokumentacja biblioteki `reqwest`](https://docs.rs/reqwest)
- [Dokumentacja biblioteki `hyper`](https://hyper.rs)
- [Jak działa HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)