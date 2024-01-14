---
title:                "Rust: Lähettämällä http-pyyntö perusautentikoinnilla"
simple_title:         "Lähettämällä http-pyyntö perusautentikoinnilla"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi
On monia syitä, miksi voit haluta lähettää HTTP-pyynnön perusautentikoinnilla. Yksi yleisimmistä syistä on, että haluat turvata pyyntösi ja varmistaa, että vain oikeutetut käyttäjät voivat käyttää sitä.

## Miten
Ei ole mitään syytä pelätä, että Rust ei sovi hyvin HTTP-pyyntöjen lähettämiseen perusautentikoinnilla. Todellisuudessa, se on hyvin yksinkertaista ja suoraviivaista! Katsotaanpa esimerkkiä siitä, miten voit lähettää HTTP-pyynnön käyttäen `reqwest`-kirjastoa ja perusautentikointia.

```rust
use reqwest::Client;
// Luodaan uusi HTTP-klientti
let client = Client::new();
// Luodaan pohja pyynnölle
let mut request = client.get("https://example.com/api/resource")
    // Lisätään perusautentikointi otsikkoon
    .basic_auth("käyttäjänimi", Some("salasana"))
    // Lähetetään pyyntö ja odotetaan vastausta
    .send()
    .await?;
// Otetaan vastauksesta tekstimuotoinen sisältö
let response_text = request.text()
    .await?;
println!("{}", response_text);
```

Tässä yksinkertaisessa esimerkissä luomme uuden HTTP-klientin ja pyyntöobjektin käyttäen `reqwest`-kirjastoa. Sitten lisäämme perusautentikointi otsikon pyyntöön ja lähetämme sen. Vastauksessa saamme tekstimuotoisen sisällön, joka tulostetaan.

## Syvällinen sukellus
HTTP-pyynnöt perusautentikoinnilla perustuvat HTTP-otsikoihin, jotka välittävät käyttäjätiedot palvelimelle. Näitä otsikoita ovat esimerkiksi `Authorization` ja `WWW-Authenticate`. Perusautentikointitietojen lisääminen pyyntöön mahdollistaa käyttäjän tunnistamisen ja tietojen turvaamisen.

On myös tärkeää huomata, että perusautentikoinnilla lähetetyt tiedot eivät ole salattuja, joten se ei ole paras vaihtoehto käytettäväksi suojaamaan arkaluontoisia tietoja.

## Katso myös
- [Reqwest](https://docs.rs/reqwest/0.11.4/reqwest/)
- [HTTP-pyyntöjen perusautentikointi (Wikipedia)](https://fi.wikipedia.org/wiki/HTTP-autentikointi#Perusautentikointi)
- [Rustin viralliset verkkosivut](https://www.rust-lang.org/fi)