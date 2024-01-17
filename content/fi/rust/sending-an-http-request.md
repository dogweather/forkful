---
title:                "Lähettäminen http-pyyntö"
html_title:           "Rust: Lähettäminen http-pyyntö"
simple_title:         "Lähettäminen http-pyyntö"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Lähettäessäsi HTTP-pyynnön, pyydät verkkopalvelimelta tietoa tai palvelua. Tämä tapahtuu esimerkiksi, kun käytät hakukonetta, lataat kuvia tai viestittelet sosiaalisessa mediassa. Koodareiden tehtävä on kirjoittaa ohjelmia, jotka ovat yhteydessä verkkopalvelimiin ja käsittelevät näitä tietopyyntöjä - tämä on yksi tärkeimmistä syistä lähettää HTTP-pyyntöjä.

## Miten:
Esimerkiksi, jos haluat lähettää GET-pyynnön, joka hakee dataa Google-hakukoneelta, voit käyttää seuraavaa esimerkkiä käyttäen Rust-ohjelmointikieltä:

```Rust
use reqwest;
let response = reqwest::get("https://www.google.com").await?;
```

Tämä luo GET-pyynnön, joka hakee Google-hakukonetta ja odottaa vastausta. Sitten voit käsitellä saamasi vastauksen, joka sisältää HTML-sisällön, kuten kuvia, tekstejä ja linkkejä.

## Syväsyvennys:
HTTP (Hypertext Transfer Protocol) on standardoitu protokolla tietoliikenteelle Webin välityksellä. Se on ollut käytössä vuodesta 1991 lähtien ja sillä on tärkeä rooli tietokonejärjestelmissä, jotka ovat yhteydessä Internetiin. Rust tarjoaa kirjastoja, kuten reqwest, joka tekee HTTP-pyyntöjen lähettämisestä helpompaa ja tehokkaampaa.

Muita vaihtoehtoja HTTP-pyyntöjen lähettämiseen Rustilla ovat esimerkiksi hyper ja curl-kirjastot. Hyper on suosittu HTTP-kirjasto Rustille ja se tarjoaa paljon ominaisuuksia, mutta se on myös hieman monimutkaisempi käyttää. Curl-kirjasto tarjoaa enemmän ominaisuuksia kuin hyödyllistä suorituskyvyn kannalta, mutta se saattaa olla vaikeampi asentaa verrattuna muihin vaihtoehtoihin.

HTTP-pyyntöjen lähettämisen taustalla olevat tärkeimmät palikat ovat TCP- ja IP-protokollat, jotka mahdollistavat tietojen siirtämisen Internetin kautta. HTTP-pyyntö koostuu erilaisista metodista, kuten GET, POST ja PUT, sekä osoitteesta ja mahdollisista kyselyparametreistä.

## Katso myös:
- [Rustin Kotisivut](https://www.rust-lang.org/) 
- [Rustin Reqwest-kirjasto](https://docs.rs/reqwest/latest/reqwest/)