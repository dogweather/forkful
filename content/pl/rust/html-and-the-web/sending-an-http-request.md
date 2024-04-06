---
date: 2024-01-20 18:00:26.799250-07:00
description: "How to: (Jak to zrobi\u0107:) Aby wys\u0142a\u0107 \u017C\u0105danie\
  \ HTTP w Rust, mo\u017Cesz u\u017Cy\u0107 popularnej biblioteki `reqwest`. Oto prosty\
  \ przyk\u0142ad."
lastmod: '2024-04-05T21:53:36.618326-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Aby wys\u0142a\u0107 \u017C\u0105danie HTTP w Rust,\
  \ mo\u017Cesz u\u017Cy\u0107 popularnej biblioteki `reqwest`."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

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
