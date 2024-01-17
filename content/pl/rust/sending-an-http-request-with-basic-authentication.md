---
title:                "Wysyłanie żądania http z podstawową uwierzytelnieniem"
html_title:           "Rust: Wysyłanie żądania http z podstawową uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawową uwierzytelnieniem"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wysyłanie zapytania HTTP z podstawową autoryzacją to sposób komunikacji między serwerem a klientem, w którym dane uwierzytelniające są przesyłane w nagłówku żądania. Programiści stosują to w celu zabezpieczenia dostępu do zasobów oraz autoryzacji użytkowników.

## Jak to zrobić:
Kodowanie przykładów i wyników wywołania w blokach kodu ```Rust...```

```Rust
use reqwest::blocking::{Client, Request};
use reqwest::StatusCode;

fn main() {
    // Tworzenie klienta HTTP
    let client = Client::new();
    // Tworzenie żądania z metodą GET i adresem URL docelowym
    let request = Request::new(reqwest::Method::GET, "https://example.com");
    // Dodawanie nagłówka uwierzytelniającego do żądania
    let request = request.header("Authorization", "Basic YWxhZGRpbjpvcGVuc2VzYW1l");
    // Wysyłanie żądania i pobieranie odpowiedzi
    let response = client.execute(request).unwrap();

    // Sprawdzanie kodu odpowiedzi
    if response.status() == StatusCode::OK {
        println!("Sukces! Uzyskano dostęp do zasobu.");
    } else {
        println!("Wystąpił błąd: {}.", response.status());
    }
}
```

## Głębszy zanurzenie:
Historia:
Wysyłanie zapytania HTTP z podstawową autoryzacją zostało wprowadzone w początkowych wersjach protokołu HTTP w latach 90. jako prosty sposób na uwierzytelnianie użytkowników. Współcześnie, jest wykorzystywane w wielu aplikacjach webowych oraz API.

Alternatywy:
Inne sposoby uwierzytelniania w protokole HTTP to m.in. uwierzytelnianie przez token, digest i NTLM. Każda z metod ma swoje zalety i wykorzystanie zależy od specyfiki projektu.

Szczegóły implementacji:
Aby wysłać żądanie z podstawową autoryzacją w języku Rust, należy użyć biblioteki reqwest, która umożliwia łatwe tworzenie i wysyłanie żądań HTTP. W przykładzie powyżej, użyliśmy metody blockingu, jednak istnieje również wersja asynchroniczna z użyciem tokio.

## Zobacz też:
- Dokumentacja biblioteki reqwest: https://docs.rs/reqwest/
- Wysyłanie zapytań HTTP w języku Rust: https://www.rust-lang.org/learn/get-started
- Porównanie różnych metod uwierzytelniania w protokole HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication