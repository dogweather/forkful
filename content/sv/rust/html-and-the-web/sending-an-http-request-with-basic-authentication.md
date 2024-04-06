---
date: 2024-01-20 18:02:41.392342-07:00
description: "Hur g\xF6r man: Exempelutdata."
lastmod: '2024-04-05T21:53:39.026085-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Skicka en HTTP-f\xF6rfr\xE5gan med Basic-autentisering"
weight: 45
---

## Hur gör man:
```Rust
use reqwest::header::{Authorization, Basic};
use std::error::Error;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let client = reqwest::Client::new();
    let user = "example_username";
    let password = "example_password";
    let auth_value = Basic { username: user.into(), password: Some(password.into()) };

    let response = client.get("https://example.com/protected")
        .header(Authorization(auth_value))
        .send()
        .await?;

    println!("Status: {}", response.status());
    println!("Body: {:?}", response.text().await?);

    Ok(())
}
```
Exempelutdata:
```
Status: 200 OK
Body: "Lyckades ansluta till den skyddade resursen."
```

## Fördjupning
HTTP Basic Authentication introducerades i HTTP/1.0-specifikationerna och har sedan dess varit ett enkelt sätt att skydda webbresurser. Det anses inte vara det säkraste metoden eftersom det baseras på obfuskerade (inte krypterade) användaruppgifter. Alternativ inkluderar OAuth, API-nycklar och JWT (JSON Web Tokens), som alla erbjuder striktare säkerhet. När du använder grundläggande autentisering i Rust, hanterar `reqwest`-paketet det mesta av komplexiteten, såsom att base64-koda autentiseringsuppgifterna, automatisera headers och hantera HTTP-sessionen.

## Se också
- Reqwest-dokumentation för att utforska mer om HTTP-klientfunktioner: [Reqwest Crates](https://docs.rs/reqwest/)
- Rusts officiella bok för att fördjupa kunskaper inom Rust: [The Rust Programming Language](https://doc.rust-lang.org/book/)
- Förståelse av HTTP-autentisering: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- Säkerhetsaspekter och bästa praxis för autentisering över HTTP: [OWASP Auth Guide](https://owasp.org/www-project-cheat-sheets/cheatsheets/Authentication_Cheat_Sheet.html)
