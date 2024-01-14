---
title:                "Rust: Sända en http-begäran med grundläggande autentisering"
simple_title:         "Sända en http-begäran med grundläggande autentisering"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Varför

Att skicka en HTTP-förfrågan med grundläggande autentisering är ett vanligt sätt att skydda webbapplikationer från oönskade användare. Att använda detta säkerhetsprotokoll ger mer kontroll över åtkomsten till en server och är en viktig del av webbutveckling.

## Hur man gör det

För att skicka en HTTP-förfrågan med grundläggande autentisering i Rust, behöver du först installera biblioteket reqwest. Detta kan enkelt göras genom att lägga till följande kod i ditt `Cargo.toml`-fil:

```
[beroenden]
reqwest = "*"
```

När du har installerat reqwest kan du börja kodandet. Här är ett enkelt exempel på hur du kan skicka en HTTP-förfrågan med grundläggande autentisering:

```
extern crate reqwest;

använda reqwest:: Invändning;

fn main() {
    låt url = "https://example.com/api";
    låt klient = Client :: nytt();
    låt svar = klient.get (url)
                      .bära_med(Authentication::basic("användarnamn", "lösenord"))
                      .skicka()
                      .förvänta sig ("Kunde inte skicka förfrågan");
   
    låt text = svar.text().expect("Kunde inte konvertera svar till text");
    println!("{}", text);
}
```

Detta kodexempel skapar en klient och skickar en GET-förfrågan till en webbadress med grundläggande autentisering. Om förfrågan lyckas kommer svaret i form av en text att skrivas ut i konsolen.

## Djupdykning

HTTP-förfrågan med grundläggande autentisering innebär att det krävs en användarnamn och lösenord för att komma åt en viss resurs på en server. Detta skapar en basnivå av säkerhet, men det finns flera andra autentiseringstekniker som kan användas för att stärka säkerheten ytterligare, exempelvis OAuth.

Det är också viktigt att notera att lösenordet som skickas vid grundläggande autentisering skickas i klartext, vilket innebär att det kan vara sårbar för avlyssning. Därför är det viktigt att använda en säker och unik lösenord för att minimera risken för obehörig åtkomst.

## Se även

- [Respwest dokumentation](https://docs.rs/reqwest/)
- [Rust officiell hemsida](https://www.rust-lang.org/sv/)
- [HTTP autentiseringstekniker jämförelse](https://medium.com/@pushpanc/understanding-http-basics-the-authorization-header-48c3041dcfb)