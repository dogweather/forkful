---
title:    "Gleam: Merkkijonon isojen kirjainten muuttaminen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Miksi: Yksi jos kaksi lausetta selittämään *miksi* joku haluaa muuttaa merkkijonon isoihin kirjaimiin.

## Miksi

Merkkijonon muuttaminen isoihin tai pieniin kirjaimiin on yksi yleisimpiä tehtäviä ohjelmoinnissa. Se on tärkeää esimerkiksi käyttäjän syötteiden käsittelyssä tai tekstin muotoilussa. Gleam tarjoaa yksinkertaisen, mutta tehokkaan tavan muuttaa merkkijonon isot tai pienet kirjaimet halutulla tavalla.

## Miten

Gleamilla merkkijonon muokkaaminen isoiksi tai pieniksi kirjaimiksi on helppoa. Se käyttää sisäänrakennettuja funktioita, joten koodin luettavuus ja tehokkuus pysyvät hyvinä. Alla esimerkki koodista ja sen tuottamasta tulosteesta:

```Gleam
fn muuta_isoksi(merkkijono) {
    return String.uppercase(merkkijono)
}

fn main() {
    let syote = "Tervetuloa Gleam-harjoitteluun!"
    let tulos = muuta_isoksi(syote)
    IO.println(tulos)
}
```

```
TERVETULOA GLEAM-HARJOITTELUUN!
```

Kuten nähdään, merkkijonon muuttaminen isoiksi kirjaimiksi tapahtuu `String.uppercase()`-funktiolla. Tässä esimerkissä funktio ottaa parametrina merkkijonon, mutta sitä voidaan soveltaa myös muilla tietotyypeillä kuten listoilla ja tupleilla.

## Syvällinen sukellus

Gleamilta löytyy myös muita sisäänrakennettuja funktioita merkkijonojen muokkaamiseen. Esimerkiksi `String.capitalize()` muuttaa merkkijonon ensimmäisen kirjaimen isoksi ja loput kirjaimet pieniksi. `String.titlecase()` puolestaan muuttaa jokaisen sanan ensimmäisen kirjaimen isoksi ja loput pieniksi.

Lisäksi Gleamin standardikirjastosta löytyy moduuli `gleam/string/transform` jolla voi suorittaa monimutkaisempia muokkauksia merkkijonolle, kuten esimerkiksi merkkien kääntämisen, leikkaamisen ja yhdistämisen. Tutustu Gleamin dokumentaatioon saadaksesi lisätietoja ja esimerkkejä.

## Katso myös

- [Gleam - virheetön ja staattisesti tyypitetty funktionaalinen ohjelmointikieli](https://gleam.run/)
- [Gleam-dokumentaatio](https://gleam.run/documentation/)
- [Gleam-standardikirjasto](https://github.com/gleam-lang/gleam_stdlib)

Kiitos, että luit tämän blogikirjoituksen ja toivottavasti se auttoi sinua ymmärtämään merkkijonojen muokkaamista Gleamilla. Jatka Gleamista oppimista ja hyödynnä sen ominaisuuksia käyttäessäsi sitä tulevissa projekteissasi.

*Kiitos lukuunpanosi tekohetkestäsi,*
Isabella, Gleam Suomi