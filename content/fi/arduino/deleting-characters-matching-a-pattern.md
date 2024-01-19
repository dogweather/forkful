---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Mallin mukaisten merkkien poistaminen tarkoittaa joidenkin tiettyjen merkkien tai merkkijonojen poistamista tekstistä tai datasta. Ohjelmoijat tekevät näin usein saadakseen datan puhtaaksi käsiteltäväksi, eli pois kaikki mistä ei ole hyötyä.

## Miten:

Koodiesimerkkejä ja näytteen tuloksia seuraavissa ```Arduino ... ``` koodilohkoissa.

```Arduino
String txt = "Hei, Maailma!";
txt.replace(" ", ""); // Poistaa kaikki välilyönnit.
Serial.println(txt); // Tulostaa "Hei,Maailma!"
```

Tämä poistaa kaikki välilyönnit tekstistä. 'Replace' funktio korvaa kaikki " " tyhjillä, ja 'Serial.println' tulostaa lopputuloksen.

```Arduino
String txt = "Hello, Maailma!";
txt.replace("ll", ""); // Poistaa kaikki 'll' merkkijonot.
Serial.println(txt); // Tulostaa "Heo, Maailma!"
```

Tässä esimerkissä 'Replace' funktio korvaa kaikki 'll' merkkijonot tyhjillä, ja 'Serial.println' tulostaa lopputuloksen.

## Syvempi sukellus:

Historiallinen konteksti - Merkkien poistaminen mallin perusteella on ollut yleistä tietokoneohjelmoinnissa jo pitkään. Arduino-ympäristö on hyödyntänyt tätä käytäntöä tarjoamalla funktioita, kuten 'Replace'.

Vaihtoehdot - Voit myös poistaa merkkejä käyttäen muita menetelmiä, kuten 'substring' tai 'charAt' funktioita, mutta 'Replace' tarjoaa yksinkertaisen ja tehokkaan tavan.

Implementaation yksityiskohdat - 'Replace' funktio tarkistaa jokaisen merkin tai merkkijonon vastaavuuden määritettyyn malliin ja vaihtaa sen. Tämä voi olla hyödyllistä, kun tarvitset erityisen merkkijonon tai merkin poistamista.

## Katso myös:

1. Arduino dokumentaatio 'Replace' funktiolle: [Arduino Replace](https://www.arduino.cc/en/Tutorial/StringReplace) 
2. Arduino Substring-funktion käyttää merkkien poistamiseen: [Arduino SubString](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
3. Katsaus eri funktioihin merkkien käsittelyssä Arduinossa: [Arduino String Functions](https://startingelectronics.org/software/arduino/learn-to-program-course/12-strings/)