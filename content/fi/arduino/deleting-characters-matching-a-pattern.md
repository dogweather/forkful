---
title:                "Samanmallisten merkkien poistaminen"
html_title:           "Arduino: Samanmallisten merkkien poistaminen"
simple_title:         "Samanmallisten merkkien poistaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Mikä on merkkijonojen poistaminen, joka vastaa malleja? Se on ohjelmointitapa poistaa ja korvata merkkijonoja tietystä kuvion tai mallin mukaan. Tätä käytetään yleensä muokkaamaan ja muokkaamaan tietoja muodossa.

Miksi ohjelmoijat tekevät sitä? Merkkijonojen poistaminen on hyödyllistä, kun haluat muuttaa tietokannan tai muiden tietokoneen tallentamien tietojen muotoa. Se voi myös auttaa voittamaan palvelunestohyökkäyksiä ja muuta ei-toivottua sisältöä.

# Kuinka?

Arduinoa voidaan käyttää poistamaan merkkijonoja, jotka vastaavat tiettyjä malleja. Tässä on yksinkertainen koodiesimerkki käyttäen funktiota ```replace()``` ja sen syötettä ```"README"```:

```
String teksti = "Tämä on README-tiedosto";
String uusiTeksti = teksti.replace("README", "Ohjeet");
Serial.println(uusiTeksti); // Tulostaa "Tämä on Ohjeet-tiedosto"
```

Käytetäänkö funktiota ```replace()``` poistaaksesi merkkijonoja tietystä mallista, joka on integer eli kokonaisluku ja neljää merkkiä lyhyempi.```

String teksti = "12345";
String uusiTeksti = teksti.replace("1234", "");
Serial.println(uusiTeksti); // Tulostaa "5"
```

Voit myös käyttää funktiota ```remove()``` Arduinoa poistamaan merkkijonoja tietystä alueelta. Katso seuraava esimerkki:

```
String teksti = "Tämä on tekstikirjaimet";
String uusiTeksti = teksti.remove(14, 7);
Serial.println(uusiTeksti); // Tulostaa "Tämä on kirjaimet"
```

# Syvä sukellus

Merkkijonojen poistaminen on ollut käytössä jo pitkään ohjelmoinnissa, ja sitä käytetään monissa eri ohjelmointikielissä. Nykyään on olemassa myös muita tapoja poistaa merkkijonoja, kuten säännölliset lausekkeet.

Arduino tarjoaa kuitenkin hyvin yksinkertaiset ja tehokkaat metodit merkkijonojen poistamiseen, ja se on erinomainen vaihtoehto, kun tarvitaan nopeaa ja yksinkertaista ratkaisua.

# Katso myös

- Säännölliset lausekkeet: https://fi.wikipedia.org/wiki/S%C3%A4%C3%A4nn%C3%B6llinen_lausers
- Arduino kirjallisuus: https://www.arduino.cc/reference/en/