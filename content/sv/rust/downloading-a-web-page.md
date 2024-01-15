---
title:                "Laddar ner en webbsida"
html_title:           "Rust: Laddar ner en webbsida"
simple_title:         "Laddar ner en webbsida"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Varför

Varför skulle du vilja ladda ner en webbsida? Det finns många användbara fall, till exempel för att skapa en offlineversion av en sida eller för att hämta data för webb-skrapning eller analys.

# Hur man gör det

För att ladda ner en webbsida i Rust, behöver du använda biblioteket Reqwest. Först måste du lägga till det som ett beroende i din `Cargo.toml` fil.

```
[dependencies]
reqwest = { version = "0.10.2", features = ["json"] }
```

Sedan kan du använda detta enkla kodexempel för att hämta webbsidan och skriva ut dess innehåll.

```
use reqwest::Client;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Skapa en klient
    let client = Client::new();

    // Ange den önskade webbsidan att hämta
    let url = "https://www.example.com";

    // Använd `get` Funktionen på klienten och skicka den önskade webbplatsen
    let resp = client.get(url).send().await?;

    // Kontrollera att svarskoden är 200 OK
    if resp.status().is_success() {
        // Hämta innehållet från svaret
        let content = resp.text().await?;

        // Skriv ut innehållet
        println!("{}", content);
    } else {
        println!("Kunde inte hämta sidan. Statuskod: {}", resp.status());
    }

    Ok(())
}
```

Detta kodexempel använder `Client` typen från Reqwest för att skapa en HTTP-anslutning till webbplatsen som du specifierar. Sedan använder den `get` funktionen för att skicka en begäran och få ett svar från webbplatsen. Du kan också lägga till andra funktioner som att ange förväntade huvuden eller använda autentisering.

# Djupdykning

Reqwest biblioteket är mycket användarvänligt och har många funktioner för att göra det möjligt att anpassa din HTTP-anslutning efter dina specifika behov. Till exempel kan du använda `.header()` funktionen för att ange anpassade huvuden i din förfrågan eller `.basic_auth()` för att använda HTTP-åtkomstautentisering.

Det finns också många andra HTTP-klientbibliotek tillgängliga i Rust, som Hyper eller Surf. Det är viktigt att välja det som passar bäst för ditt projekt och tillåter de funktioner som du behöver.

# Se också

- [Officiell Reqwest dokumentation](https://docs.rs/reqwest)
- [Rust: Bygg ett webbskrapningsverktyg](https://dev.to/mcnelsonph/build-a-web-scraper-with-rust-6fn)