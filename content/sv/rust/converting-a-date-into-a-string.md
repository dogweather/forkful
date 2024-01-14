---
title:    "Rust: Konvertera ett datum till en sträng"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Varför

Att konvertera ett datum till en sträng är en vanlig uppgift vid programmering, särskilt när man arbetar med datum och tid. Oavsett om du behöver presentera datumet i ett visst format eller spara det som en sträng, är det viktigt att veta hur man utför denna konvertering i Rust.

## Hur man gör

Ett enkelt sätt att konvertera ett datum till en sträng i Rust är att använda DateTime-modulen från standardbiblioteket. Först måste du importera DateTime-klassen och sedan skapa ett nytt DateTime-objekt med önskat datum. Sedan kan du använda DateTime-objektets formatfunktion för att konvertera datumet till en sträng med ett visst format.

En kodexempel skulle se ut såhär:

```rust
use std::time::DateTime;

let date = DateTime::parse_from_rfc3339("2020-01-01T12:00:00+00:00").unwrap();
let date_str = date.format("%d-%m-%Y").to_string();

println!("{}", date_str); // output: 01-01-2020
```

I det här exemplet har vi använt formatet "%d-%m-%Y" för att få datumet att visas som dag-månad-år. Du kan självklart byta detta format baserat på dina behov och preferenser.

Det är också värt att nämna att det finns flera externa bibliotek tillgängliga för att hjälpa till med datumkonverteringar i Rust, som chrono och time. Dessa bibliotek erbjuder olika funktioner och formatalternativ, så det är bra att undersöka vilket som passar bäst för dina specifika behov.

## Djupdykning

Vid konvertering av datum till strängar är det viktigt att förstå skillnaden mellan en lokaliserad och en icke-lokaliserad sträng. En lokaliserad sträng är specifik för ett visst språk eller region, medan en icke-lokaliserad sträng är opåverkad av språk och region.

När du använder formatfunktionen i DateTime-objektet, bör du vara medveten om vilken typ av sträng du vill ha som output. Om du till exempel vill ha en lokaliserad sträng för ett svenskt datum kan du använda "%A, den %d %B %Y".

För att utföra mer avancerade datumkonverteringar, som att extrahera enskilda komponenter från ett datum (t.ex. månad eller år), finns det också flera inbyggda funktioner tillgängliga i DateTime-objektet.

## Se även

Det finns många andra aspekter av datum och tid i Rust som kan vara intressanta att lära sig mer om. Här är några användbara resurser att titta på:

- [Rust Standard Library - DateTime](https://doc.rust-lang.org/std/time/struct.DateTime.html)
- [Rust by Example - Date and Time](https://doc.rust-lang.org/stable/rust-by-example/std_misc/time.html)
- [Chrono](https://github.com/chronotope/chrono)
- [Time](https://github.com/chronotope/time)

Lycka till med att konvertera datum till strängar i Rust!