---
title:                "Rust: Lähettämällä http-pyyntö"
simple_title:         "Lähettämällä http-pyyntö"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi
HTTP-pyynnön lähettäminen on tärkeä osa monia verkkosovelluksia ja -palveluita. Se mahdollistaa tietojen hakemisen ja lähettämisen eri verkkosivustoilta ja -palveluilta.

## Miten
Ensimmäiseksi meidän täytyy lisätä `reqwest` kirjasto `Cargo.toml` -tiedostoon. Tämä voidaan tehdä helposti suorittamalla seuraava komento Rust projektin juurella:

```
cargo add reqwest
```

Seuraavaksi meidän täytyy tuoda `reqwest` kirjasto koodiin `main.rs` tiedostossa:

```
use reqwest;
```

Nyt me voimme luoda HTTP-pyynnön `reqwest::get` funktion avulla ja tallentaa sen muuttujaan `response`:

```
let response = reqwest::get("https://www.example.com")
    .expect("Could not send request");
```

Voimme tulostaa pyynnön vastauksen staattinen sisältö seuraavasti:

```
println!("Response: {}", response.text().unwrap());
```

Tämän tulisi tulostaa verkkosivun sisältö terminaalissa.

## Syvemmälle
HTTP-pyynnön lähettäminen sisältää monia eri osia, kuten URL-osoitteen luomisen, otsikkojen määrittämisen ja pyynnön tekemisen. `reqwest` kirjasto ottaa tämän työn hoitaakseen puolestamme, joten meidän ei tarvitse huolehtia niistä.

On myös tärkeää huomata, että `reqwest` kirjasto käyttää `async` ja `await` syntaksia, mikä tarkoittaa sitä, että pyyntö tehdään asynkronisesti. Tämä mahdollistaa muiden tehtävien suorittamisen samalla, kun odotamme vastausta pyynnöltä.

## Katso myös
- [Rustin asennus ja ensimmäinen projekti](https://www.example.com/rust-installation)
- [Async ja await selitettynä](https://www.example.com/async-await-explained)
- [reqwest kirjaston dokumentaatio](https://www.example.com/reqwest-docs)