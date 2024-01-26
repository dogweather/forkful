---
title:                "Virheiden käsittely"
date:                  2024-01-26T00:53:01.148052-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheiden käsittely"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/handling-errors.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Virheenkäsittely tarkoittaa sellaisten tilanteiden ennakointia, joissa ohjelmakoodisi ei toimi odotetusti ja näiden tilanteiden hallittua käsittelyä. Ohjelmoijat tekevät näin, koska se pitää sovellukset robusteina ja käyttäjäystävällisinä, jopa odottamattomien tapahtumien edessä.

## Kuinka:
Gleamissa käytät usein `Result`-tyyppiä virheenkäsittelyyn. Se on enum, jolla on kaksi varianttia: `Ok` (onnistumiselle) ja `Error` (epäonnistumiselle). Tässä on yksinkertainen esimerkki:

```Gleam
pub fn might_fail(break_it: Bool) -> Result(Int, String) {
  if break_it {
    Error("Hupsista! Se meni rikki.".to_string())
  } else {
    Ok(42)
  }
}

pub fn main() {
  let result = might_fail(False)
  case result {
    Ok(arvo) => arvo
    Error(viesti) => {
      io.println(viesti)
      0
    } 
  }
}
```

Jos ajat `main` funktion `might_fail(False)` kanssa, se palauttaa `42`. Jos annat arvon `True`, se tulostaa "Hupsista! Se meni rikki." ja palauttaa `0`.

## Syväsukellus
Gleamin virheenkäsittelytapa on vaikutteita saanut sen Erlang-juurista. Historiallisesti Erlang käyttää "anna sen kaatua" -filosofiaa, luottaen valvontapuihin prosessien vikojen hallinnassa. Kuitenkin, kun kirjoitat Gleam-koodia, joka ei ole tarkoitettu valvotuksi prosessiksi, kuten kirjaston funktiossa, haluat käsitellä virheitä eksplisiittisesti.

Vaihtoehtoina `Result`-tyypin käytölle on `Option`-tyypin käyttö tapauksissa, joissa jokin saattaa olla `None` (ei mitään) tai `Some` (jotakin), mutta nämä eivät sisällä virhetietoja. Virheiden signaloimiseksi prosessirajojen yli saatat käyttää Erlangin viestinlähetysmekanismeja.

Gleamin virheenkäsittely heijastaa funktionaalisen ohjelmoinnin tyyliä, jossa sivuvaikutukset (kuten virheet) hallitaan tyyppien ja mallien vastaavuuden avulla, tarjoten selkeyttä ja ennustettavuutta virheiden hallintaan.

## Katso Myös
- [Erlangin Virheenkäsittely](http://erlang.org/doc/reference_manual/errors.html)