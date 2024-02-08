---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
aliases:
- sv/rust/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:41.392342-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att inkludera användarnamn och lösenord i en HTTP-header för att få tillgång till skyddade resurser. Programmerare gör detta för att interagera med webbtjänster som kräver användarverifiering.

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
