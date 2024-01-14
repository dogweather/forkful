---
title:    "Gleam: Merkkijonon isojen kirjainten muotoilu"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi

Kapitalisointi on tärkeä osa ohjelmointia, ja se auttaa tekstin selkeyttämisessä ja yhtenäistämisessä. Se on hyödyllinen esimerkiksi kun halutaan muuttaa syötteen muotoa tai näyttää merkkijonoja käyttäjille. Gleam-ohjelmointikieli tarjoaa helpon ja tehokkaan tavan suorittaa stringien kapitalisointia, mikä tekee siitä ihanteellisen vaihtoehdon tähän tarkoitukseen.

## Kuinka tehdä

Kapitalisointi Gleam-kielellä onnistuu helposti käyttämällä sisäänrakennettuja funktioita. Oletetaan, että meillä on muuttuja `teksti` joka sisältää merkkijonon "tämä on teksti". Voimme käyttää `String.capitalize` funktiota muuttaaksemme tekstin ensimmäisen kirjaimen isoiksi kirjaimiksi:

```Gleam
teksti
|> String.capitalize
```

Tämä tuottaa tuloksen "Tämä on teksti". Voimme myös käyttää `String.to_uppercase` funktiota muuttaaksemme koko tekstin isoiksi kirjaimiksi:

```Gleam
teksti
|> String.to_uppercase
```

Tämä tuottaa tuloksen "TÄMÄ ON TEKSTI". Näiden funktioiden lisäksi Gleam tarjoaa myös muita hyödyllisiä toimintoja stringien manipulointiin, kuten `String.to_lowercase` ja `String.to_titlecase`.

## Syvemmälle

Kapitalisointia käyttäessämme voimme myös tarvittaessa muuttaa tietyn osan merkkijonosta. Voimme käyttää `String.replace` funktiota vaihtaaksemme tietyn merkkijonon osan toiseen. Esimerkiksi, jos haluamme korvata "teksti" sanalla "koodi", voimme käyttää seuraavaa koodia:

```Gleam
teksti
|> String.replace("teksti", "koodi")
|> String.capitalize
```

Tämä tuottaa tuloksen "Tämä on koodi". Lisäksi voimme käyttää `String.contains` funktiota tarkistaaksemme, sisältääkö merkkijono tietyn osan. Tämä on erittäin hyödyllistä, kun haluamme tehdä kapitalisointiä vain tietynlaisissa tapauksissa.

## Katso myös

- [Gleam Ohjelmointikielen kotisivu](https://gleam.run/)
- [Gleam Ohjelmointikielen oppaat](https://gleam.run/getting-started)
- [Kapitalisoinnin dokumentaatio Gleamissa](https://gleam.run/core/String.html#functions)