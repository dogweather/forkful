---
title:                "Rust: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie zapytania HTTP jest ważnym elementem każdej aplikacji internetowej. Pozwala na komunikację z innymi serwerami, zbieranie danych i wyświetlanie ich użytkownikom. Dzięki używaniu Rusta, możemy mieć pewność, że nasze zapytania będą bezpieczne i wydajne.

## Jak to zrobić

```rust
use std::io::Read;
use hyper::{Client, Uri};

//tworzenie klienta HTTP z biblioteki Hyper
let client = Client::new();

//tworzenie URI z adresu strony, do której chcemy wysłać zapytanie
let uri = Uri::from_static("https://example.com");

//tworzenie zapytania GET
let request = client.get(uri);

//wysłanie zapytania i przechwycenie odpowiedzi
let mut response = request.send().unwrap();

//odczytanie odpowiedzi i wyświetlenie jej w terminalu
let mut buffer = String::new();
response.read_to_string(&mut buffer).unwrap();
println!("{}", buffer);
```

Przykładowy output: "Hello, World!" (zawartość strony https://example.com)

## Głębsze zagłębienie

Istnieją różne metody wysyłania zapytań HTTP w Rust. Możemy używać biblioteki standardowej `std::net::TcpStream`, jednak biblioteki zewnętrzne takie jak Hyper czy Req są bardziej zaawansowane i łatwiejsze w użyciu. Te biblioteki umożliwiają również obsługę protokołu HTTPS, co jest niezwykle ważne dla bezpiecznego przesyłania danych w sieci.

Rust jest językiem kompilowanym, co oznacza, że ​​kod jest przetwarzany przed uruchomieniem. Dzięki temu możemy uniknąć błędów w czasie wykonania i mieć pewność, że nasza aplikacja będzie działać poprawnie.

## Zobacz również

- [Dokumentacja Hyper](https://docs.rs/hyper/)
- [Poradnik: Wysyłanie żądań HTTP w Rust](https://blog.logrocket.com/sending-http-requests-in-rust/)
- [Dokumentacja biblioteki standardowej: std::net::TcpStream](https://doc.rust-lang.org/std/net/struct.TcpStream.html)