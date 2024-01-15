---
title:                "Nykyisen päivämäärän saaminen"
html_title:           "Rust: Nykyisen päivämäärän saaminen"
simple_title:         "Nykyisen päivämäärän saaminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Nykyisen päivämäärän hakemisessa on hyötyä monissa tilanteissa, kuten sovellusten ajastamisessa, tapahtumien aikaleimoissa ja käyttäjän suoranaisessa tilantarkkailussa.

## Kuinka

```Rust
use std::time::{SystemTime, UNIX_EPOCH}; // tarvittavat kirjastot tuodaan alussa

// järjestelmän kellonaika UNIX-aikana käyttäen SystemTime ja UNIX_EPOCH -kirjastoja
let now = SystemTime::now();
let seconds = now.duration_since(UNIX_EPOCH).expect("Ongelma aikaleimoissa.").as_secs();

// nykyisen päivämäärän muotoilu käyttäen strftime-syntaxia 
let date = time::strftime("%d-%m-%Y", &time::now()).unwrap();

println!("UNIX-aikana: {}", seconds);
println!("Päivämäärämuodossa: {}", date);

// Output:
// UNIX-aikana: 1604734707
// Päivämäärämuodossa: 07-11-2020
```

## Syventävä tieto

Päivämäärän haku tuntuu yksinkertaiselta toiminnolta, mutta taustalla tapahtuu paljon. Esimerkiksi UNIX-aika ilmaisee ajan kulun vuoden 1970 alusta lähtien sekunteina, kun taas strftime-syntax antaa mahdollisuuden muotoilla aikaleiman halutunlaiseksi. Rustissa käytettävät SystemTime ja UNIX_EPOCH -kirjastot mahdollistavat monipuolisen aikaleimojen käsittelyn.

## Katso myös

- [Rustin virallinen dokumentaatio päivämäärän hakuun](https://doc.rust-lang.org/std/time/fn.strftime.html)
- [Date and time libraries for Rust](https://blog.logrocket.com/date-and-time-libraries-for-rust/)
- [Using Times and Dates in Rust](https://blog.skylight.io/using-times-and-dates-in-rust/)