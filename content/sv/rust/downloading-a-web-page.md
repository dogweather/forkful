---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida innebär att hämta sidans HTML-kod och andra data. Programmerare gör detta för att analysera innehållet, spara det för senare, eller bearbeta det på olika sätt.

## Hur man gör:

Här visar vi ett enkelt sätt att ladda ner en webbsida med Rust, specifikt med `reqwest`-biblioteket. Vi skapar en asynkron funktion som gör en GET-förfrågan till en angiven URL och skickar tillbaka innehållet som en sträng.

```Rust
use reqwest;

async fn ladda_ner_webbsida(url: &str) -> Result<String, reqwest::Error> {
    let innehall = reqwest::get(url).await?.text().await?;
    Ok(innehall)
}
```

Du kan nu använda den här funktionen för att hämta HTML-innehåll från en webbsida. Exemplen nedan visar hur du kan göra för att prova detta.

```Rust
#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let url = "https://www.exemple.se";
    let resultat = ladda_ner_webbsida(url).await?;
    println!("{}", resultat);
    Ok(())
}
```

## Fördjupning 

Att ladda ner webbsidor är inte något nytt - det är basfunktionalitet i webbläsare och det har länge varit ett nyckelverktyg för webbskrapning. Alternativ till `reqwest` inkluderar andra bibliotek som `hyper` och `curl`.

Den fullständiga processen för att ladda ner en webbsida innefattar att skapa en HTTP-förfrågan, skicka den till servern som webbsidan ligger på, och sedan ta emot och tolka serverns svar - vilket vanligtvis är webbsidans HTML-kod.

## Se också

För mer information om webbskrapning och HTTP-förfrågningar med Rust, se följande källor:

- [Rust reqwest dokumentation](https://docs.rs/reqwest/0.10.10/reqwest/)
- [Webbläsare och Rust](https://arewebrowseryet.org/)
- [Rust hyper bibliotek](https://hyper.rs/)
- [Rust curl bibliotek](https://docs.rs/curl/0.4.38/curl/)