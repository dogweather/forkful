---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
html_title:           "Rust: HTTP-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Lähettäessäsi HTTP-pyynnön perusautentikoinnilla, lähetät pyyntösi salasanan ja käyttäjätunnuksen kanssa. Tämä autentikointimenetelmä auttaa suojaamaan tietojasi ja varmistamaan, että vain oikeat käyttäjät pääsevät verkkosivustoon tai sovellukseen.

## Kuinka:

Käytä alla olevia esimerkkejä lähettääksesi HTTP-pyynnön perusautentikoinnilla Rustilla. Ensimmäisessä esimerkissä näet, kuinka lisätä otsakkeeseen tarvittavat käyttäjätunnus ja salasana. Toisessa esimerkissä näet, kuinka käyttää ```Reqwest``` kirjaston ```basic_auth``` -metodia lähettääksesi pyynnön.

```Rust
use reqwest::blocking::Client;

fn main() {
    let client = Client::new();

    let response = client
        .get("https://www.example.com")
        .header("Authorization", "Basic dXNlcm5hbWU6cGFzc3dvcmQ=")
        .send()
        .unwrap();
}
```

```Rust
use reqwest;

fn main() -> Result<(), reqwest::Error> {
    let response = reqwest::blocking::get("https://www.example.com")?
        .basic_auth("username", Some("password"))
        .send()?;

    // tee jotain vastauksella tässä
}
```

## Syväsukellus:

Perusautentikointia käytettiin alun perin HTTP: n käyttäjätunnistuksessa, jossa käyttäjän piti syöttää salasana ja käyttäjätunnus jokaiseen pyyntöön. Kuitenkin nykyään monet käyttävät kehittyneempiä autentikointimenetelmiä, kuten OAuthia. Tämän lisäksi on myös muita tapoja lähettää HTTP-pyyntöjä, kuten Digest-autentikointi ja TLS.

## Katso myös:

Voit lukea lisää perusautentikoinnista ja muista autentikointimenetelmistä Reqwestin virallisesta dokumentaatiosta osoitteessa: https://docs.rs/reqwest/0.10.5/reqwest/struct.RequestBuilder.html#autentikaatio-metodit 

Lisää tietoa HTTP-autentikoinnin historiasta löydät täältä: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication 

Ja jos haluat tutustua muihin tapoihin lähettää HTTP-pyynnöt Rustilla, tutustu tähän oppaaseen: https://wiki.rust-lang.org/lto-HTTP