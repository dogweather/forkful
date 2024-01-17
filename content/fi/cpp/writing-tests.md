---
title:                "Testien kirjoittaminen"
html_title:           "C++: Testien kirjoittaminen"
simple_title:         "Testien kirjoittaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-tests.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Kirjoittaminen testejä on tärkeä osa ohjelmointia. Tarkoituksena on varmistaa, että koodi toimii odotetulla tavalla ja havaita mahdolliset virheet ennen kuin ohjelma julkaistaan. Tämä auttaa parantamaan ohjelman laatua ja vähentää mahdollisia bugeja, mikä säästää aikaa ja vaivaa pitkällä aikavälillä.

## Kuinka:
Käytä testikirjastoja, kuten Catch2 tai Google Test, kirjoittaaksesi testejä ohjelmallesi. Tämän avulla voit kirjoittaa pieniä yksikkötestejä, jotka testaavat yksittäisiä osia koodistasi. Seuraava esimerkki käyttää Catch2-kirjastoa testataksesi funktiota, joka laskee kahden kokonaisluvun summan.
```C++
TEST_CASE("Lasketaan kahden kokonaisluvun summa", "[laskeminen]") {
   REQUIRE(laskeSumma(2, 3) == 5);
}
```
Tämä testi varmistaa, että funktio palauttaa oikean summan laskiessaan kahden kokonaisluvun summan. Jos testi epäonnistuu, se osoittaa ongelman koodissa, ja voit korjata sen ennen kuin julkaiset ohjelmasi.

## Syväsukellus:
Kirjoittaminen testeihin tuli suosituksi lähestymistavaksi ohjelmistokehityksessä 1980-luvulla, kun yhä enemmän ohjelmia alkoi olla suuria ja monimutkaisia. Testien lisääminen auttoi kehittäjiä tunnistamaan ja korjaamaan virheitä nopeammin, mikä paransi ohjelmien laatua ja stabiilisuutta. Vaikka testien kirjoittaminen saattaa tuntua aikaa vievältä alkuvaiheessa, se säästää aikaa ja vaivaa myöhemmin. On myös muita tapoja testata ohjelmaa, kuten manuaalinen testaus, mutta niitä ei voida automatisoida ja ne ovat alttiimpia inhimillisille virheille.

## Katso myös:
- [Catch2-kirjasto](https://github.com/catchorg/Catch2)
- [Google Test -kirjasto](https://github.com/google/googletest)
- [Ohjelmistotestauksen perusteet](https://www.cs.helsinki.fi/u/tukimuu/tite/k07/luennot/06/Testaus/materiaali/Testaus.pdf)