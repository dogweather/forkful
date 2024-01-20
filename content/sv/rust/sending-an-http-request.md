---
title:                "Att skicka en http-begäran"
html_title:           "Go: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran är att be om bestämd data från en server genom Internet. Programmerare gör detta för att hämta, uppdatera eller radera information på andra system utan att behöva återskapa den på egen hand. 

## Så här gör du:
Här är ett exempel på hur man skickar en HTTP GET-begäran i Rust med hjälp av `reqwest` crate.

```Rust
extern crate reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::get("https://www.rust-lang.org").await?;

    println!("{}", response.status());
    Ok(())
}
```

Om allt fungerar som det ska, bör utmatningen se ut så här:

```
200 OK
```

## Djupdykning
Historisk sett har programmerare skickat HTTP-begäran länge, redan innan Rust fanns. Det finns alternativ till `reqwest` som `hyper` och `surf`, men `reqwest` är populär för sitt enkla, hög-nivå API.

Vad gäller implementeringen omvandlar Rust's typsystem HTTP-begäran till ett säkert och enkelt format.
Vad mer? Rusts asynkrona egenskaper göra det möjligt för begäran att ske parallellt, vilket förbättrar prestandan.

## Se även
- [Rusts officiella webbplats](https://www.rust-lang.org)
- [reqwest API-dokumentation på docs.rs](https://docs.rs/reqwest)
- [Mer om HTTP-statuskoder](https://developer.mozilla.org/sv-SE/docs/Web/HTTP/Status)
- [Alternativ till reqwest: `Hyper` biblioteket](https://hyper.rs/)
- [Alternativ till reqwest: `Surf` biblioteket](https://docs.rs/surf)