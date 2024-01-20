---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Wysyłanie żądań HTTP w Rust 

## Co i dlaczego?

Wysyłanie żądań HTTP to proste jak bułka z masłem, polega na nawiązaniu połączenia z serwerem i wysłaniu do niego wiadomości. Programiści robią to na co dzień, aby komunikować się z usługami internetowymi.

## Jak to zrobić:

W Rust, wysyłanie żądań HTTP jest proste dzięki bibliotece `reqwest`. Zaczniemy od dodania zależności do naszego `Cargo.toml`:
```Rust
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

Następnie napiszmy prosty kod, który wysyła żądanie GET:
```Rust
#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("https://httpbin.org/ip")
        .await?
        .text()
        .await?;
    println!("response: {}", response);
    Ok(())
}
```

Gdy uruchomisz ten program, powinieneś zobaczyć respons z Serwera.

## Głębsze spojrzenie

Wysłanie żądania HTTP jest jednym z podstawowych zadań w pracy każdego programisty. Stosowane jest do komunikacji z serwerem, a także do wymiany danych między różnymi usługami internetowymi.

W przeszłości, do wysyłania takich zapytań najczęściej używano języka `bash` i narzędzia `curl`. Dziś, z pomocą nowoczesnych języków programowania (takich jak Rust) i ich pakietów, jesteśmy w stanie osiągać to samo w bardziej czytelny i efektywny sposób.

Warto też pamiętać, że używamy biblioteki `reqwest` do wykonania żądania HTTP, ale Rust posiada wiele alternatywnych bibliotek do tego celu, takich jak `hyper` czy `surfer`.

## Zobacz także:

1. [Dokumentacja reqwest](https://docs.rs/reqwest/0.11.3/reqwest/)
2. [Dokumentacja tokio](https://docs.rs/tokio/1.5.0/tokio/)
3. [Dokumentacja async Rust](https://rust-lang.github.io/async-book/01_getting_started/04_async_await_primer.html)