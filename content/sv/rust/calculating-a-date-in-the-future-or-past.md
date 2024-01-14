---
title:    "Rust: Beräkning av ett datum i framtiden eller det förflutna"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller det förflutna kan vara en användbar funktion i många olika programmeringsuppgifter. Det kan användas för att planera möten eller evenemang, hantera deadlines eller helt enkelt hålla koll på tiden.

## Hur man gör

Att beräkna ett datum i framtiden eller förflutna är enkelt med hjälp av Rusts standardbibliotek. Först bör vi definiera ett datum med hjälp av `NaiveDate`-strukturen:

```Rust
let start_date = NaiveDate::from_ymd(2020, 11, 1);
```

Vi kan sedan använda funktionen `checked_add` för att lägga till ett antal dagar till detta datum för att få ett datum i framtiden:

```Rust
let future_date = start_date.checked_add(Duration::days(14)).unwrap();
```

Om vi istället vill ha ett datum i förflutna, kan vi använda funktionen `checked_sub`:

```Rust
let past_date = start_date.checked_sub(Duration::weeks(3)).unwrap();
```

Notera att vi använder oss av `checked_add` och `checked_sub` för att undvika eventuella fel om det skulle bli ett ogiltigt datum.

## Djupdykning

Som vi nämnde tidigare är det möjligt att använda en `Duration` för att lägga till eller subtrahera dagar, veckor, månader eller till och med år från ett datum. Detta är på grund av att interna representationen av ett datum i Rust är en enkel räknare på antalet dagar sedan `1 januari 1` och därmed kan en `Duration` enkelt läggas till eller subtraheras för att få ett nytt datum.

Det finns också andra funktioner som `checked_add_signed` och `checked_sub_signed` som kan användas för att beräkna datum baserat på ett angivet antal dagar, även om det är negativt.

## Se även

* [Dokumentation för Rusts standardbibliotek](https://doc.rust-lang.org/std/index.html)
* [Officiell bok för Rust](https://doc.rust-lang.org/book/)