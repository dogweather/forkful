---
title:                "Rust: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Cześć czytelnicy! Witamy w kolejnym wpisie z serii programowania w języku Rust. Dzisiaj przyjrzymy się tematowi pobierania stron internetowych. Możesz zapytać: dlaczego chciałbym pobrać stronę internetową? Jedną z najczęstszych przyczyn jest zbieranie danych do analizy lub wykorzystania ich w aplikacjach. Ale niezależnie od powodu, jest to przydatna umiejętność do posiadania w swoim zestawie narzędzi. Zatem zaczynajmy!

## Jak to zrobić

Aby pobrać stronę internetową w języku Rust, musimy użyć biblioteki `reqwest`. Najpierw zdeklarujmy zależność w pliku `Cargo.toml`:

```
[dependencies]
reqwest = { version = "0.11.5", features = ["blocking", "json"] }
```

Następnie w pliku `main.rs` zaimportujmy bibliotekę i użyjmy funkcji `get` do pobrania adresu URL:

```
use reqwest;
use std::io::Read;

fn main() -> Result<(), reqwest::Error> {
    let mut response = reqwest::get("https://example.com")?; // pobierz URL
    let mut buff = String::new();
    // odczytaj dane do zmiennej buff
    response.read_to_string(&mut buff)?;
    println!("Otrzymano: {}", buff); // wyświetl pobraną stronę
    Ok(())
}
```

Po uruchomieniu powyższego kodu, powinieneś zobaczyć w konsoli cały kod HTML ze strony internetowej. To już wszystko! Pozwala to na podstawowe pobieranie stron internetowych, ale możesz też użyć biblioteki `scraper` do parsowania i wybierania konkretnych danych z pobranej strony.

## Głębokie zanurzenie

Jeśli chcesz pogłębić swoją wiedzę na temat pobierania stron internetowych w języku Rust, możesz zacząć od zrozumienia jak działa HTTP i jakie są różnice między bibliotekami `reqwest` i `hyper`. Możesz też przeczytać dokumentację biblioteki `scraper` i spróbować wybrać konkretne elementy z pobranej strony.

## Zobacz także

- [dokumentacja reqwest](https://docs.rs/reqwest/)
- [dokumentacja scraper](https://docs.rs/scraper/)
- [Poradnik Rust: pobieranie i przetwarzanie HTML](https://dev.to/taliamax/parsing-html-in-rust-4k42)
- [Jak pobrać stronę internetową z użyciem Rust](https://www.onwebsecurity.com/securing-personal-data/rust-download-page-with-https/)

Dziękujemy za przeczytanie naszego wpisu. Mam nadzieję, że zdobyłeś trochę wiedzy na temat pobierania stron internetowych w języku Rust. Do zobaczenia w kolejnym wpisie!