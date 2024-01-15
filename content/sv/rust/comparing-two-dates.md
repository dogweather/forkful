---
title:                "Jämförelse av två datum"
html_title:           "Rust: Jämförelse av två datum"
simple_title:         "Jämförelse av två datum"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Jag tror att alla kan relatera till den frustrerande uppgiften att jämföra två datum. Det är inte alltid så enkelt som att skriva ut dem och se vilket som är större eller mindre än det andra.

Men oroa dig inte, med Rusts inbyggda funktioner kan du enkelt jämföra två datum och få exakt det resultatet du behöver.

## Så här

För att jämföra två datum i Rust behöver du bara använda dig av `Date`-modulen. Du kan skapa två instanser av `Date` med hjälp av `from_y/m/d`-funktionen och sedan jämföra dem med antingen `<` eller `>`-operatorerna.

Se till att importera `Date`-modulen först genom att skriva:

```Rust
use std::time::Date;
```

Sedan kan du skapa dina två datum och jämföra dem i en `if`-sats:

```Rust
let start_date = Date::from_y_m_d(2021, 1, 1);
let end_date = Date::from_y_m_d(2021, 1, 9);

if start_date < end_date {
    println!("Startdatumet är mindre än slutdatumet");
} else {
    println!("Startdatumet är större än slutdatumet");
}
```

Beroende på vilka datum du skrivit in kommer antingen `"Startdatumet är mindre än slutdatumet"` eller `"Startdatumet är större än slutdatumet"` att skrivas ut.

## Djupdykning

Det finns flera saker att tänka på när man jämför två datum i Rust. Först och främst, om datumet har ett annat tidsformat som timmar eller minuter så kommer det att ignoreras vid jämförelsen. Bara datumet i sig är relevant.

För det andra, om du vill jämföra två datum baserade på veckonummer eller dag i veckan, då kan du använda `to_weekday()` och `to_weekday_num()`-funktionerna för att få ut den specifika informationen från datumet.

För mer djupgående information om hur man jämför datum i Rust, ta en titt på [Rusts dokumentation](https://doc.rust-lang.org/std/time/struct.Date.html).

## Se även

* [Rusts dokumentation för datum](https://doc.rust-lang.org/std/time/struct.Date.html)
* [En översikt över Rusts grundläggande datatyper](https://www.aidanwoods.com/blog/a-tour-of-rust-basic-types/)
* [Jämförelse av datum i andra programmeringsspråk](https://www.sitepoint.com/compare-dates-time-stamps/)