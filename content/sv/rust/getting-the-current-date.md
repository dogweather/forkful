---
title:    "Rust: Att få den aktuella datumen"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför?

Att kunna hämta och använda dagens datum är en viktig del av många programmerares vardagliga arbete. Oavsett om du behöver logga aktiviteter eller hålla koll på tidsstämplar, är det viktigt att ha en pålitlig metod för att få fram det aktuella datumet. I denna bloggpost kommer vi att titta på hur man kan göra detta i Rust.

## Hur gör man?

För att få fram dagens datum i Rust finns det flera olika metoder, men den enklaste och mest pålitliga är att använda standardbiblioteket. Först och främst måste vi importera standardbibliotekets `chrono` modul med hjälp av följande kod:

```Rust
use chrono::{DateTime, Utc};
```

Sedan kan vi skapa ett nytt `DateTime` objekt med hjälp av `Utc::now()` funktionen:

```Rust
let now: DateTime<Utc> = Utc::now();
```

Genom att använda `now` variabeln som ett argument i `format()` funktionen kan vi sedan få ut datumbeskrivningen som en `String`:

```Rust
let current_date = now.format("%Y-%m-%d").to_string();
```

När vi kör koden kommer outputen att vara dagens datum i formatet "ÅÅÅÅ-MM-DD", till exempel "2021-01-23". Detta är en enkel och pålitlig metod för att få fram dagens datum i Rust.

## Deep Dive

I bakgrunden använder `Utc::now()` funktionen systemklockan för att få fram det aktuella datumet och tiden. Därefter konverteras det till UTC-tiden, som är en standardiserad tidszon som används av många programmeringsspråk. Genom att använda formatstringen "%Y-%m-%d" berättar vi för `format()` funktionen att vi vill ha ut datumbeskrivningen i årtal, månad och dag format. Detta gör det enkelt att anpassa outputen till önskat format.

## Se även

- [Chrono Dokumentation](https://docs.rs/chrono/latest/chrono/)
- [Rust Standardbibliotek](https://doc.rust-lang.org/std/)
- [Datum- och Tidsbibliotek i Rust](https://www.theswiftlangblog.com/how-to-get-the-current-date-and-time-in-rust/)