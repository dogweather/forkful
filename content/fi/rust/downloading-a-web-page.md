---
title:                "Verkkosivun lataaminen."
html_title:           "Rust: Verkkosivun lataaminen."
simple_title:         "Verkkosivun lataaminen."
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Lataaminen tai hankkiminen web-sivulta on prosessi, jossa ohjelmisto hakee ja tallentaa tiettyä sivua internetistä. Tätä tarvitaan esimerkiksi silloin kun halutaan näyttää web-sivun sisältöä sovelluksessa tai tallentaa sitä myöhemmin offline-käyttöä varten.

## Miten:

```Rust
// Lataa web-sivu annetusta URL-osoitteesta
use reqwest::blocking::Response;

fn main() {
    let response: Response = reqwest::blocking::get("https://www.example.com").unwrap();

    println!("Statuskoodi: {}", response.status());
    println!("Otsikko: {:?}", response.headers().get("content-type"));
    println!("Body: {}", response.text().unwrap());
}
```
Tulostus:
```
Statuskoodi: 200 OK
Otsikko: Some("text/html; charset=utf-8")
Body: <html>
<head>
  <title>Esimerkki</title>
</head>
<body>
  <h1>Tervetuloa!</h1>
  <p>Tämä on esimerkkisivu.</p>
</body>
</html>
```

## Syväsukellus:
Lataaminen on ollut tärkeä osa ohjelmointia jo vuosikymmenten ajan, mutta nykyään se on yhä tärkeämpää kun meillä on valtava määrä tietoa saatavilla internetissä. On olemassa monia tapoja ladata web-sivuja, mutta Rustin reqwest-kirjasto on yksi suosituimmista. Se tarjoaa helpon ja vakaan tavan käsitellä HTTP-kutsuja ja vastauksia.

## Katso myös:
- [reqwest-kirjaston dokumentaatio](https://docs.rs/reqwest)
- [Rust-ohjelmointikielen virallinen kotisivu](https://www.rust-lang.org/fi)
- [HTTP-protokollan perusteet](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)