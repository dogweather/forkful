---
date: 2024-01-20 18:02:43.912205-07:00
description: "Jak to zrobi\u0107: Je\u015Bli wszystko jest ok, zobaczysz w konsoli\
  \ \"Odpowied\u017A:\" i tre\u015B\u0107 zasobu. W przypadku b\u0142\u0119du, pojawi\
  \ si\u0119 \"B\u0142\u0105d:\" i kod statusu."
lastmod: '2024-04-05T21:53:36.621059-06:00'
model: gpt-4-1106-preview
summary: "Je\u015Bli wszystko jest ok, zobaczysz w konsoli \"Odpowied\u017A:\" i tre\u015B\
  \u0107 zasobu."
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## Jak to zrobić:
```rust
use reqwest::header::{Authorization, Basic};
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let client = reqwest::Client::new();
    let username = "moj_login";
    let password = "moje_haslo"; // W praktyce, użyj lepszego sposobu na przechowywanie haseł.

   let auth = Basic {
       username: username.to_string(),
       password: Some(password.to_string()),
   };

    let response = client
        .get("http://przykladowa.strona/zasob")
        .header(Authorization(auth))
        .send()
        .await?;

    if response.status().is_success() {
        let content = response.text().await?;
        println!("Odpowiedź: {}", content);
    } else {
        println!("Błąd: {}", response.status());
    }
    
    Ok(())
}
```
Jeśli wszystko jest ok, zobaczysz w konsoli "Odpowiedź:" i treść zasobu. W przypadku błędu, pojawi się "Błąd:" i kod statusu.

## Deep Dive
Podstawowa autentykacja HTTP (Basic Auth) to jedna z metod uwierzytelniania określona w HTTP/1.0. Wykorzystywana od lat 90., ale przez podstawowy poziom zabezpieczeń (base64) nie jest polecana dla wrażliwych danych bez HTTPS. Alternatywą może być Digest Auth, OAuth lub inne rozwiązanie z tokenem. W Rust, popularne są biblioteki takie jak `reqwest` dla żądań HTTP, które ułatwiają dodawanie nagłówków autentykacji.

## Zobacz także:
- [Dokumentacja `reqwest`](https://docs.rs/reqwest/)
- [Specyfikacja Basic Auth](https://tools.ietf.org/html/rfc7617)
