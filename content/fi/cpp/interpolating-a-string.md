---
title:                "Merkkijonon interpolointi"
html_title:           "C++: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Merkkijonon interpolointi tarkoittaa merkkijonon sisällön sisällyttämistä toiseen merkkijonoon. Tämä tehdään usein tarpeeseen lisätä dynaamisesti muuttuvia tietoja merkkijonoon, kuten käyttäjän syötteitä tai tietokannasta haettuja arvoja. Ohjelmoijat käyttävät interpolointia lisätäkseen joustavuutta ja tehokkuutta heidän koodissaan.

## Miten:
Alla on kaksi esimerkkiä kuinka interpolointi toimii ```C++``` koodissa:

### Esimerkki 1:
```C++
std::string nimi = "Maija";
std::cout << "Hei, " << nimi << "! Tervetuloa!" << std::endl;
```
**Tulostus:** Hei, Maija! Tervetuloa!

### Esimerkki 2:
```C++
std::cout << "Lisäämme kaksi lukua " << 10 << " ja " << 5 << ". Lopputulos on " << 10+5 << std::endl;
```
**Tulostus:** Lisäämme kaksi lukua 10 ja 5. Lopputulos on 15.

## Syvempi sukellus:
Merkkijonon interpolointia käytettiin ensimmäisen kerran runsaat 40 vuotta sitten BASIC-ohjelmointikielessä. Nykyään eri ohjelmointikielissä käytetään erilaisia tapoja interpoloida merkkijonoja, mutta perusidea on sama. Joissakin kielissä, kuten Pythonissa, interpolointi tapahtuu käyttämällä %-merkkiä ja muuttujia. Toisissa kielissä, kuten ```C++```, voidaan käyttää << -operaattoria tai erillistä funktiota. Lisäksi on olemassa myös muita tapoja, kuten string.format-menetelmä, joka on yleinen Java-ohjelmointikielessä.

Interpolointiin on myös muita vaihtoehtoja, kuten string concatenation eli merkkijonojen yhdistäminen, mutta se ei ole yhtä tehokas kuin interpolointi. Merkkijonojen interpolointi on myös hyödyllinen tapa tehdä koodista helpommin luettavaa ja ymmärrettävää.

Interpolointi ```C++``` kielessä onnistuu eri tavoilla, kuten käyttämällä std::stringstream-luokkaa tai käyttämällä sabluunakehyksen std::format-funktiota. Jokaisella tavalla on omat hyötynsä ja haittansa, ja voit valita sen mukaan mikä sopii parhaiten sinun tarkoitukseesi.

## Katso myös:
- [Merkkijonon interpolointi (Wikipedia)](https://fi.wikipedia.org/wiki/Merkkijonon_interpolointi)
- [BASIC-kaarin ja HOLBASIC (Hackaday)](https://hackaday.com/2017/05/02/basics-and-holbasic-your-first-dirty-hack-to-keep-your-kids-away-from-coding/)
- [Interpolating strings (C++ Reference)](https://en.cppreference.com/w/cpp/language/string_literal#Interpolating_strings)
- [String.format-menetelmä (Oracle Docs)](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#format(java.lang.String,%20java.lang.Object...))