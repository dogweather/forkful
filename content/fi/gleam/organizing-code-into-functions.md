---
title:                "Koodin järjestäminen funktioihin"
date:                  2024-01-26T01:10:40.223329-07:00
model:                 gpt-4-1106-preview
simple_title:         "Koodin järjestäminen funktioihin"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Koodin järjestäminen funktioihin tarkoittaa ohjelman toiminnallisuuden jakamista pienempiin, uudelleenkäytettäviin osiin. Ohjelmoijat tekevät näin selkeyttääkseen koodia, parantaakseen ylläpidettävyyttä ja välttääkseen toistoa.

## Kuinka:
Tässä on yksinkertainen esimerkki koodin järjestämisestä funktioihin Gleamilla:

```gleam
fn add(x, y) {
  x + y
}

fn main() {
  let sum = add(3, 4)
  sum
}

// Esimerkkitulostus
// 7
```

Tässä katkelmassa `add` on funktio, joka ottaa kaksi arvoa ja summaa ne. `main` on kohta, jossa kutsutaan `add`-funktiota ja käsitellään tulos.

## Syväsukellus
Historiallisesti funktioiden (tai 'alirutiinien') käsite mullisti ohjelmoinnin ja raivasi tietä rakenteelliselle ohjelmoinnille 1960-luvulta lähtien. Funktiot kannustavat modulaariseen lähestymistapaan, jossa ongelmat jaetaan aliongelmiin, ratkaistaan itsenäisesti ja koostetaan suuremman ongelman ratkaisemiseksi.

Gleamissa, joka on vahvasti tyypitetty, funktiot sisältävät myös tyyppitiedot, varmistaen niiden käytön yhdenmukaisuuden määritelmänsä kanssa. Tämä vähentää virheitä ja selkeyttää tarkoitusperiä.

Vaihtoehto funktioille sisältää suoraan koodauksen, jossa logiikka kirjoitetaan toistuvasti uudelleen. Vaikka se voi joskus olla nopeampaa pienille, kertaluonteisille tehtäville, suoraan koodaus ei skaalaudu hyvin suurempiin sovelluksiin.

Funktioiden järjestämistä koskeviin toteutusyksityiskohtiin voi kuulua funktionaalinen koostaminen, jossa funktioita käytetään rakennuspalikoina, ja korkeamman järjestyksen funktiot, jotka ottavat muita funktioita argumenteiksi tai palauttavat niitä, lisäten joustavuutta siihen, miten koodi järjestetään ja suoritetaan.

## Katso myös
Lisää funktioista Gleamissa voit sukeltaa viralliseen dokumentaatioon:
- [Gleamin kielen funktiot](https://gleam.run/book/tour/functions.html)

Tai tutkia laajempia ohjelmointikäsitteitä:
- [Mozilla Developer Network JavaScript-funktiot](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions)
- [Learn You Some Erlang for Great Good! - Moduuleista ja funktioista](https://learnyousomeerlang.com/modules)