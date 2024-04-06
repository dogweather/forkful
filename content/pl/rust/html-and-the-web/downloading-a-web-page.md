---
date: 2024-01-20 17:44:52.012652-07:00
description: "How to: (Jak to zrobi\u0107:) Pobieranie stron webowych to nic nowego.\
  \ W dawnych czasach u\u017Cywano `wget` lub `curl` w terminalu. W Rust, wykorzystujemy\
  \ bardziej\u2026"
lastmod: '2024-04-05T22:50:49.488188-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Pobieranie stron webowych to nic nowego."
title: Pobieranie strony internetowej
weight: 42
---

## How to: (Jak to zrobić:)
```rust
// Dołącz bibliotekę "reqwest" do Cargo.toml
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    // Pobieranie strony
    let url = "http://example.com";
    let response = reqwest::get(url).await?;

    // Weryfikacja czy żądanie się powiodło
    println!("Status: {}", response.status());

    // Wypisanie zawartości strony
    let body = response.text().await?;
    println!("Body:\n{}", body);

    Ok(())
}
```
Wyjście:
```
Status: 200 OK
Body:
Contents of the web page
```

## Deep Dive (Głębsze spojrzenie)
Pobieranie stron webowych to nic nowego. W dawnych czasach używano `wget` lub `curl` w terminalu. W Rust, wykorzystujemy bardziej rustykalne podejście. `reqwest` to popularna, asynchroniczna biblioteka do wykonywania żądań HTTP. Alternatywną opcją jest `hyper`, bardziej niskopoziomowa, która daje więcej kontroli, ale jest też bardziej skomplikowana w użyciu. Kiedy używasz `reqwest`, ważne jest, aby obsłużyć błędy oraz ewentualnie zgłosić je w przyszłości.

## See Also (Zobacz również)
- Oficjalna dokumentacja `reqwest`: https://docs.rs/reqwest/
- Asynchroniczność w Rust: https://rust-lang.github.io/async-book/
- Porównanie bibliotek HTTP dla Rust: https://www.arewewebyet.org/topics/libraries/#http-clients
