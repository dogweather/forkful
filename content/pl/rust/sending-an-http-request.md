---
title:                "Wysyłanie żądania HTTP"
date:                  2024-01-20T18:00:26.799250-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Wysyłanie żądania HTTP to po prostu pytanie, które komputer zadaje serwerowi w sieci. Programiści robią to, by pobrać dane, wysłać informacje lub komunikować się z usługami webowymi.

## How to: (Jak to zrobić:)
Aby wysłać żądanie HTTP w Rust, możesz użyć popularnej biblioteki `reqwest`. Oto prosty przykład:

```Rust
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("https://api.github.com/repos/rust-lang/rust").await?;
    
    println!("Status: {}", response.status());
    println!("Headers:\n{:#?}", response.headers());
    
    let body = response.text().await?;
    println!("Body:\n{}", body);
    
    Ok(())
}
```

Wynik działania:
```
Status: 200 OK
Headers:
...
Body:
{ ... }
```

## Deep Dive (W głąb tematu):
Pierwsze kroki wysyłania żądań HTTP w Rust mogły używać `hyper`, niskopoziomowej biblioteki. `Reqwest` to zbudowana na `hyper` biblioteka zapewniająca wyższy poziom abstrakcji. Istnieją alternatywy takie jak `surf` w ekosystemie async, ale `reqwest` często jest wybierany za bogactwo funkcji i łatwość użycia. Aby skorzystać z biblioteki `reqwest`, musisz także użyć `tokio`, asynchronicznego środowiska uruchomieniowego, ponieważ `reqwest` jest asynchroniczny.

## See Also (Zobacz też):
- [Dokumentacja `reqwest`](https://docs.rs/reqwest/)
- [Dokumentacja `hyper`](https://docs.rs/hyper/)
- [The Rust Async Book](https://rust-lang.github.io/async-book/)
- [Dokumentacja `surf`](https://docs.rs/surf/)
