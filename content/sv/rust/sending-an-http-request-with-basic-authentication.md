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

## Vad & Varför?
Att skicka en HTTP-förfrågan med grundläggande autentisering är ett sätt för utvecklare att säkert kommunicera med en webbtjänst. Autentisering används för att verifiera en användares identitet och skydda känslig information från obehöriga. Det är en vanlig praxis för att integrera externa tjänster i applikationer.

## Så här gör du:
Här är ett enkelt exempel på hur du kan skicka en HTTP-förfrågan med grundläggande autentisering i Rust:
```
use reqwest;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let client = reqwest::blocking::Client::new();
    let mut res = client
        .get("https://example.com")
        .basic_auth("username", Some("password"))
        .send()?;

    println!("Statuskod: {}", res.status());
    
    Ok(())
}
```

Exempelutmatningen visar statuskoden för förfrågan, som i detta fall är förväntat svar 200 OK.

## Djupdykning:
För att förstå hur man skickar en HTTP-förfrågan med grundläggande autentisering är det viktigt att förstå historien bakom det. År 1995 skapades grundläggande autentisering som en del av HTTP-standarderna för att ge en enkel metod för autentisering. Det finns dock alternativ till grundläggande autentisering, som OAuth och JWT, som erbjuder en mer robust och säker autentisering.

I Rust används biblioteket Reqwest för att skicka HTTP-förfrågningar. Genom att använda metoden `.basic_auth()` och tillhandahålla användarnamn och lösenord i parametrarna kan en förfrågan med grundläggande autentisering skickas.

## Se även:
- Rust Reqwest bibliotekets dokumentation: https://docs.rs/reqwest
- Mer information om grundläggande autentisering: https://developer.mozilla.org/sv/docs/Web/HTTP/Authentication#indroduction