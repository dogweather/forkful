---
title:                "Rust: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Niektóre API wymagają uwierzytelnienia za pomocą podstawowej autoryzacji HTTP przed udostępnieniem dostępu do swoich danych. W tym artykule dowiesz się, dlaczego i jak wysyłać żądanie HTTP z podstawowym uwierzytelnieniem w języku Rust.

## Jak To Zrobić

```Rust
use reqwest::blocking::{Client, Response};
use reqwest::header::{AUTHORIZATION, BASIC};
use std::io::Read;

fn main() {
    let url = "https://api.example.com/data";
    let client = Client::new();
    
    let mut response = client.get(url)
        .header(AUTHORIZATION, BASIC, "username:password")
        .send().expect("Nie udało się wysłać żądania");
    
    let mut content = String::new();
    response.read_to_string(&mut content)
        .expect("Nie udało się odczytać odpowiedzi");
    
    println!("Odpowiedź: {}", content);
}
```

### Przykładowy wynik

```
Odpowiedź: Dostęp do danych
```

## Głębsze Wnioskowanie

Podstawowa autoryzacja HTTP polega na przesyłaniu w nagłówku żądania danych uwierzytelniających w bazie kodowania. W przypadku języka Rust można skorzystać z biblioteki `reqwest`, która umożliwia wysyłanie żądań HTTP w sposób prosty i bezpieczny.

Aby wysłać żądanie z podstawowym uwierzytelnieniem, należy utworzyć instancję klienta `Client` i użyć funkcji `get()` z ustawieniami URL oraz nagłówkiem `AUTHORIZATION` z wartością `BASIC` i odpowiednimi danymi logowania. Następnie można użyć funkcji `send()` do wysłania żądania i odczytać odpowiedź za pomocą funkcji `read_to_string()`. W ten sposób można uzyskać dostęp do potrzebnych danych.

## Zobacz również

- Dokumentacja biblioteki `reqwest`: [https://docs.rs/reqwest/0.10.0/reqwest/](https://docs.rs/reqwest/0.10.0/reqwest/)
- Informacje o podstawowej autoryzacji HTTP: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)