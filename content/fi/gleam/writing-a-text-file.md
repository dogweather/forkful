---
title:    "Gleam: Tekstitiedoston kirjoittaminen"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi kirjoittaa tekstitiedostoa Gleam-ohjelmointikielellä? Yksinkertaisesti sanottuna, tekstitiedostot ovat olennainen osa lähes jokaista ohjelmointiprojektia. Ne voivat sisältää kaikenlaisia tietoja, kuten käyttäjätietoja, konfiguraatiotietoja ja loki-tietoja. Kirjoittamalla tekstitiedoston Gleamilla voit tehokkaasti hallita ja muokata näitä tietoja koodissa.

## Miten

```Gleam
tiedostonimi = "tiedosto.txt"
teksti = "Tervetuloa lukemaan Gleam-ohjelmointiblogia!"
Gleam.File.write(tiedostonimi, teksti)

```

Tämä yksinkertainen koodiesimerkki osoittaa, kuinka voit käyttää Gleamia kirjoittamaan tekstitiedoston. Ensimmäisessä rivissä määritämme muuttujan "tiedostonimi", johon tallennamme tekstitiedoston nimen. Seuraavassa rivissä määritämme muuttujan "teksti", johon tallennamme itse tekstin, jonka haluamme kirjoittaa tiedostoon. Lopuksi käytämme Gleam.File.write-funktiota, jotta tekstitiedosto kirjoitetaan ja tallennetaan käyttämällä aiempia määriteltyjä muuttujia. Tämän jälkeen voit avata luomasi tiedoston ja näet tulosteen:

```
Tervetuloa lukemaan Gleam-ohjelmointiblogia!
```

## Syvällinen sukellus

Tiedostojen kirjoittaminen voi olla hyödyllistä monissa erilaisissa ohjelmointitilanteissa. Voit esimerkiksi tallentaa käyttäjien syöttämiä tietoja ja käyttää niitä myöhemmin, kirjoittaa lokitiedostoja, jotka auttavat sinua seuraamaan ohjelmistosi suoritusta tai tallentaa muutoksia konfiguraatiotietoihin helpottaaksesi mahdollisten muutosten tekemistä myöhemmin.

Kun kirjoitat tekstitiedostoa, sinun on myös tärkeää ottaa huomioon, että tekstiä voidaan lisätä ja poistaa. Tämä tarkoittaa, että tekstiä ei välttämättä lisätä aivan tiedoston alkuun, vaan se voidaan lisätä myös johonkin muuhun kohtaan tiedostossa. Gleamilla tämä voidaan tehdä käyttämällä Gleam.File.append-funktiota.

```
teksti = " Tämä teksti lisätään tiedoston loppuun."
Gleam.File.append(tiedostonimi, teksti)
```

Tällä tavoin voit lisätä uuden tekstin alkuperäisen tekstin loppuun sen sijaan, että kirjoitat sen kokonaan uudelleen.

## Katso myös

- [Gleam käyttäjädokumentaatio](https://gleam.run/documentation/)
- [Gleamin virallinen GitHub-sivu](https://github.com/gleam-lang/gleam)