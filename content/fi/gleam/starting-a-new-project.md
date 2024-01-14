---
title:    "Gleam: Aloittaminen uusi hanke"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Miksi aloittaa uusi Gleam-projekti?

On monia syitä aloittaa uusi Gleam-projekti. Ensinnäkin, Gleam on kieli, joka on suunniteltu toimimaan tehokkaasti ja luotettavasti. Se tarjoaa myös helpon tavan käsitellä rinnakkaisuutta ja monikäyttöisyyttä. Lisäksi Gleamilla on vahva type-järjestelmä, mikä tekee koodista turvallisempaa ja vähentää mahdollisia virheitä.

## Kuinka aloittaa

Aloittaaksesi uuden projektin Gleamilla, sinun tulee ensin asentaa Gleam-kääntäjä ja -ympäristö. Tee tämä komennolla ```brew install gleam``` tai seuraa tarkempia asennusohjeita Gleam-sivustolta. Kun olet asentanut Gleamin, voit luoda uuden projektin komennolla ```gleam new project_name```. Tämä luo pohjan uudelle projektillesi, jossa on valmiina muun muassa kaikki tarvittavat tiedostot ja rakenteet.

Seuraavaksi voit aloittaa koodaamisen kirjoittamalla Gleamilla koodia editoriisi ja tallentamalla sen .gleam-tiedostoon. Voit ajaa koodisi komennolla ```gleam build``` ja suorittaa sen komennolla ```gleam run```. Jos kaikki sujui hyvin, näet konsolissa tulostettavan tekstin "Hello World!", mikä on Gleam-projektin standarikoodi.

## Syväsukellus

Aloittaessasi uuden projektin Gleamilla, on hyvä tuntea muutama peruskäsite. Gleamissa käytetään moduuleja, jotka ovat koodinpätkiä, joita voidaan käyttää sellaisenaan muiden koodien kanssa. Moduuleilla on myös moduulitason funktioita, jotka ovat saman moduulin sisällä olevia funktioita. Moduulit voivat myös olla riippuvaisia toisista moduuleista, jotka on määritelty ```gleam.toml```-tiedostossa.

Gleamissa on myös monia muita käsitteitä, kuten tyyppien ja algebran tyyppien käyttö sekä rinnakkaisuuden ja muistin hallinnan käsitteet. Näihin kannattaa tutustua tarkemmin Gleam-dokumentaatiosta.

# Katso myös

- Official Gleam-sivusto: https://gleam.run/
- Gleam-dokumentaatio: https://gleam.run/documentation/
- Gleam-esimerkkejä: https://github.com/gleam-lang/gleam/tree/master/examples