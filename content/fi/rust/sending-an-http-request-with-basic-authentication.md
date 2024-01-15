---
title:                "Perusautentikoinnin käyttäminen http-pyynnöllä"
html_title:           "Rust: Perusautentikoinnin käyttäminen http-pyynnöllä"
simple_title:         "Perusautentikoinnin käyttäminen http-pyynnöllä"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi?

Jos haluat lähettää HTTP-pyynnön, jossa on perusautentikointi, se voi johtua siitä, että haluat suojata tietoja tai haluat käyttää rajoittuneita resursseja palvelimella.

## Miten?

Tässä on yksinkertainen esimerkki siitä, miten lähetät HTTP-pyynnön Rust-kielellä perusautentikoinnilla.

```Rust
use reqwest;

fn main() {
    let username = "käyttäjänimi";
    let password = "salasana";
    let client = reqwest::Client::new();
    let res = client.get("http://www.example.com")
        .basic_auth(username, Some(password))
        .send()
        .unwrap();
    println!("Vastaus: {}", res.status());
}
```

Tulostuksena pitäisi olla "Vastaus: 200 OK", mikä tarkoittaa, että pyyntö onnistui ja palvelin palautti odotetun vastauksen.

## Syvällinen sukellus

Perusautentikointi on yksi tapa suojata HTTP-pyynnöt ja vastaukset. Se vaatii käyttäjänimen ja salasanan lähettämisen jokaisessa pyynnössä. Toisin sanoen, jokaisen HTTP-pyynnön mukana on "Authorization" -otsake, jossa käyttäjänimi ja salasana ovat Base64-koodattuna.

Tämä autentikointitapa ei ole turvallisin vaihtoehto, sillä Base64-koodaus on helppo purkaa ja käyttäjänimet ja salasanat saattavat päätyä vääriin käsiin. Parempi vaihtoehto on käyttää esimerkiksi SSL-sertifikaatteja tai OAuth-autentikointia.

## Katso myös

- [Reqwest dokumentaatio](https://docs.rs/reqwest)
- [HTTP-autentikointityypit](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Rust-ohjelmointikielen kotisivu](https://www.rust-lang.org/)