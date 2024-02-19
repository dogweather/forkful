---
aliases:
- /pl/rust/downloading-a-web-page/
date: 2024-01-20 17:44:52.012652-07:00
description: "Pobieranie strony internetowej to proces \u015Bci\u0105gania jej zawarto\u015B\
  ci. Programi\u015Bci robi\u0105 to, by analizowa\u0107 dane, monitorowa\u0107 zmiany,\
  \ czy integrowa\u0107 us\u0142ugi."
lastmod: 2024-02-18 23:08:49.388864
model: gpt-4-1106-preview
summary: "Pobieranie strony internetowej to proces \u015Bci\u0105gania jej zawarto\u015B\
  ci. Programi\u015Bci robi\u0105 to, by analizowa\u0107 dane, monitorowa\u0107 zmiany,\
  \ czy integrowa\u0107 us\u0142ugi."
title: Pobieranie strony internetowej
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Pobieranie strony internetowej to proces ściągania jej zawartości. Programiści robią to, by analizować dane, monitorować zmiany, czy integrować usługi.

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
