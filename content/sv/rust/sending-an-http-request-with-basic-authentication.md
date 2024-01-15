---
title:                "Sända en http-begäran med grundläggande autentisering"
html_title:           "Rust: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

#Varför

För att kommunicera med en server via HTTP, behöver du ibland autentisera dina anrop för att verifiera din identitet. Genom att skicka en HTTP-förfrågan med grundläggande autentisering kan du på ett enkelt och överskådligt sätt hantera och verifiera autentiseringsprocessen.

##Så här gör du

För att skicka en HTTP-förfrågan med grundläggande autentisering i Rust, följ dessa steg:

1. Importera biblioteket `reqwest` i början av din kod.

```rust
use reqwest;
```

2. Skapa en `Client` för att hantera anropet till servern.

```rust
let client = reqwest::Client::new();
```

3. Konstruera en `RequestBuilder` genom att ange URL:n till servern.

```rust
let builder = client.get("www.example.com");
```

4. Lägg till autentiseringsinformationen i form av en användarnamn och lösenordskombination.

```rust
builder.basic_auth("användarnamn", Some("lösenord"));
```

5. Skicka förfrågan med `send()` och få ett `Response` tillbaka.

```rust
let response = builder.send().await?;
```

6. Hantera och använd svaret från servern.

```rust
let body = response.text().await?;
println!("{}", body);
```

##Fördjupning

HTTP-förfrågningar med grundläggande autentisering fungerar genom att skicka autentiseringsuppgifterna i en `Authorization` header som består av autentiseringsprotokollet (i detta fall "Basic") och en Base64-kodad sträng av användarnamn och lösenord. Detta behöver göras för varje enskild förfrågan till servern.

##Se även

- [rust-reqwest paket](https://crates.io/crates/reqwest)
- [HTTP autentisering](https://developer.mozilla.org/sv/docs/Web/HTTP/Authentication)