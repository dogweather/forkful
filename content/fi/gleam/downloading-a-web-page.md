---
title:                "Verkkosivun lataaminen"
html_title:           "Gleam: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Kun puhutaan verkkosivun lataamisesta, tarkoitetaan sitä, että tietokone hakee ja näyttää tietyn verkkosivun sisällön käyttäjälle. Tähän voi sisältyä esimerkiksi kuvien, tekstien ja HTML-koodin lataaminen. Näin verkkosivuilla oleva tieto saadaan näkyviin ja käsiteltäväksi.

Ohjelmoijat lataavat verkkosivuja useista eri syistä. Se voi auttaa heitä keräämään dataa, kuten uutisia, ja työstämään sitä edelleen. Lisäksi se voi myös auttaa heitä testaamaan sivujen toimivuutta ja tarkistamaan virheitä. Yleisesti ottaen verkkosivujen lataaminen on välttämätöntä monille ohjelmointitoiminnoille, jotka liittyvät uusimpien tietojen käyttämiseen ja käsittelemiseen.

## Miten:
Jos haluat ladata verkkosivun Gleamilla, sinun tulee käyttää moduulia `http_` ja sen toimintoa `get`. Tämä lähettää HTTP GET -pyynnön määrittämäsi URL-osoitteeseen ja palauttaa vastauksen sekä mahdollisen virhekoodin. Seuraava koodi esimerkki näyttää, miten tämä tapahtuu:
```
Gleam import http_

let response =
    http_.get("https://www.example.com")

case response {
    Ok(resp) -> {
        // käsittele vastaus tässä
    }
    Error(code) -> {
        // käsittele virhekoodi tässä
    }
}
```

Jos kaikki onnistuu, vastauksena saat `Ok` -arvon mukana kaikki tarvittavat tiedot, kuten HTTP-tilakoodin, otsikot ja vastauksen rungon. Sen sijaan, jos vastaanotat virhekoodin, se tulee `Error` -arvon mukana ja voit käsitellä sen haluamallasi tavalla.

## Syvällinen sukellus:
HTTP-pyyntöjen lähettämiseen on olemassa useita vaihtoehtoja, kuten `httpc` ja `hackney` -moduulit. Nämä tarjoavat lisäominaisuuksia ja mukavampia tapoja käsitellä vastauksia.

Verkkosivujen lataamiseen voi myös käyttää muita tekniikoita, kuten web-skrapingia ja REST APIen kutsumista. Näitä on kuitenkin hyvä käyttää harkiten ja vastuullisesti, jotta ei aiheuteta liikaa kuormitusta ja häiriöitä kyseisellä verkkosivustolla.

HTTP-kielen tausta on hyödyllistä ymmärtää syvemmin, jotta voit hyödyntää sitä tehokkaasti. HTTP on protokolla, jota käytetään kommunikoimaan tietokoneiden välillä, jotta tiedot voidaan välittää selaimillemme. Se on myös perusta monille muille internetin tekniikoille, kuten web-selainohjelmoinnille ja web-sivustojen toiminnalle.

## Katso myös:
- HTTP-moodulin dokumentaatiot: https://gleam.run/modules/http.
- Katso esimerkkejä HTTP-pyyntöjen lähettämisestä: https://github.com/gleam-lang/gleam_stdlib_examples/tree/master/http.