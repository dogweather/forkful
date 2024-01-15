---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "Rust: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach coraz więcej aplikacji wymaga autoryzacji użytkownika. Przesyłanie żądań HTTP z uwierzytelnianiem podstawowym jest jednym z najprostszych sposobów na zapewnienie bezpieczeństwa podczas komunikacji z serwerem. W tym artykule dowiesz się, jak za pomocą Rusta wykonać takie żądanie i otrzymać pożądany wynik.

## Jak wykonać

Rust jest językiem programowania, który jest idealny dla tworzenia szybkich i bezpiecznych aplikacji sieciowych. Aby wykonać żądanie HTTP z uwierzytelnianiem podstawowym, wystarczy użyć biblioteki reqwest.

```Rust
use reqwest::blocking::Client;

let username = "example_username";
let password = "example_password";

let client = Client::new();
let response = client.get("https://example.com")
    .basic_auth(username, Some(password))
    .send()
    .expect("Nie udało się wysłać żądania");

println!("Kod odpowiedzi: {}", response.status());

let body = response.text()
    .expect("Nie udało się odczytać zawartości");

println!("Zawartość odpowiedzi: {}", body);
```

Powyższy kod tworzy nowego klienta żądań, ustawiając jednocześnie nazwę użytkownika i hasło do uwierzytelnienia. Następnie wysyła żądanie GET na wskazany adres URL i oczekuje odpowiedzi. W przypadku sukcesu, kod odpowiedzi oraz zawartość odpowiedzi są wyświetlane na ekranie.

## Deep Dive

Podstawowe uwierzytelnianie w protokole HTTP polega na przesyłaniu nazwy użytkownika i hasła w nagłówku "Authorization" żądania. W naszym przykładzie użyliśmy metody `basic_auth` z biblioteki reqwest, która automatycznie tworzy odpowiedni nagłówek. Jeżeli jednak chciałbyś zbudować go ręcznie, musisz dodać prefiks "Basic" przed zakodowanym ciągiem znaków `base64` zawierającym nazwę użytkownika i hasło oddzielone dwukropkiem.

Ponadto, należy pamiętać, że podstawowe uwierzytelnianie nie jest bezpieczne, ponieważ nazwa użytkownika i hasło są przesyłane w postaci niezaszyfrowanej. Dlatego też powinno być stosowane tylko w przypadku wyraźnej potrzeby i zawsze powinno być zastępowane bardziej bezpiecznymi metodami uwierzytelniania.

## Zobacz także

- [Dokumentacja biblioteki reqwest](https://docs.rs/reqwest/)
- [Wykorzystanie nagłówka "Authorization" w protokole HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [Bezpieczeństwo uwierzytelniania w protokole HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)