---
title:    "C++: Tekstin etsiminen ja korvaaminen"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi: Miksi etsiä ja korvata tekstiä

Tekstin etsiminen ja korvaaminen on tärkeä osa ohjelmointia, koska se auttaa parantamaan koodin luettavuutta ja tehokkuutta. Se voi myös säästää aikaa ja vaivaa, kun muutoksia tarvitsee tehdä useisiin tiedostoihin tai tehdä samanlaisia muutoksia useita kertoja koodissa.

## Kuinka: Esimerkkejä koodista ja tulosteista

Etsimistä ja korvaamista varten C++ -kieltä käytetään usein aliohjelmassa nimeltä "replace". Tässä esimerkissä näytämme, kuinka korvataan kaikki "hello" -tekstit "hei" -sanoilla:

```C++
string teksti = "hello world!";
replace(teksti.begin(), teksti.end(), "hello", "hei");
cout << teksti << endl;
```
Tuloste: "hei world!"

Voit myös etsiä ja korvata tiettyjä sanoja tai merkkijonoja, kuten tässä esimerkissä, jossa korvaamme kaikki numerot nollalla:

```C++
string teksti = "Ostoslistalla on 2 omenaa, 1 banaani ja 3 porkkanaa.";
replace(teksti.begin(), teksti.end(), "2", "0");
replace(teksti.begin(), teksti.end(), "1", "0");
replace(teksti.begin(), teksti.end(), "3", "0");
cout << teksti << endl;
```
Tuloste: "Ostoslistalla on 0 omenaa, 0 banaani ja 0 porkkanaa."

## Syventävä tarkastelu: Tietoa tekstien etsimisestä ja korvaamisesta

C++:ssa tekstien etsiminen ja korvaaminen perustuu pääasiassa standardikirjastoon, johon kuuluu useita hyödyllisiä aliohjelmia ja funktioita, kuten "find" ja "replace". Nämä aliohjelmat ovat erittäin tehokkaita ja voivat käsitellä erilaisia datatyyppejä.

On myös tärkeää muistaa, että tekstiin kohdistuvat muutokset ovat usein ei-iteratiivisia, mikä tarkoittaa, että koodi lukee ja muokkaa tiedostoa vain kerran, eikä tarvitse käydä läpi samaa tekstiä useita kertoja.

## Katso myös

- [C++ standardikirjasto](https://www.cplusplus.com/reference/)
- [Text replacement in C++](https://www.educative.io/edpresso/text-replacement-in-cpp)
- [Replace string in C++](https://www.geeksforgeeks.org/replace-string-cpp/)