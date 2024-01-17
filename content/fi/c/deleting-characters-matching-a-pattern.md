---
title:                "Tietokoneohjelmointi: Hahmojen poistaminen tiettyä kaavaa vastaavilta"
html_title:           "C: Tietokoneohjelmointi: Hahmojen poistaminen tiettyä kaavaa vastaavilta"
simple_title:         "Tietokoneohjelmointi: Hahmojen poistaminen tiettyä kaavaa vastaavilta"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Character pattern matchingilla tapahtuu merkkien poistaminen, jotka vastaavat tiettyä kaavaa. Tämä on hyödyllistä, koska se mahdollistaa tietyn tyyppisten merkkien nopean ja tehokkaan poistamisen koodista. Se myös tekee koodin lukemisesta ja ylläpidosta helpompaa, mikä säästää aikaa ja vaivaa.

## Miten tehdä?

Esimerkiksi, jos haluamme poistaa kaikki välilyönnit merkkijonosta, voimme käyttää seuraavaa koodinpätkää:

```C
char* str = "Tämä on esimerkkimerkkijono.";
char* new_str = str_replace(str, " ", ""); 
printf("%s", new_str); // Tulostaa "Tämäonesimerkkimerkkijono."
```

Tässä koodissa käytämme str_replace-funktiota, joka korvaa kaikki välilyönnit tyhjällä merkillä. Tämä tekee uudesta merkkijonosta "Tämäonesimerkkimerkkijono."

## Syväsukellus

Historiallisessa kontekstissa, merkkien poistaminen vastaavien kaavojen avulla on ollut tärkeä osa ohjelmointia jo pitkään. Aiemmin se nähtiin tarpeellisena, koska monet järjestelmät eivät pystyneet käsittelemään erikoismerkkejä tai otaksuivat tiettyjä kaavoja.

Nykyään on myös olemassa muita tapoja poistaa merkkejä, kuten suoraan käyttäen merkkijonojen manipulointifunktioita tai käyttämällä säännöllisiä lausekkeita. Jokaisella lähestymistavalla on omat etunsa ja haittansa, joten on tärkeää valita oikea menetelmä tilanteen mukaan.

Koodin toteutuksesta riippuen merkkien poistamisen tehokkuus voi vaihdella. Esimerkiksi, jos käytetään suuria merkkijonoja, kannattaa miettiä muita vaihtoehtoja koodin nopeuttamiseksi. On myös tärkeää huolehtia, että merkkien poistamisen yhteydessä ei vahingoiteta muita osia koodista.

## Katso myös

- [Merkkijonojen manipulointifunktiot C:ssä](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Säännölliset lausekkeet C:ssä](https://www.tutorialspoint.com/c_standard_library/regex_h.htm)