---
title:                "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi 
On monia tapauksia, joissa Arduino-ohjelmoinnin yhteydessä on tarpeen muuttaa tekstiä pieniksi kirjaimiksi. Esimerkiksi käyttäjän syötteiden yhdenmukaistaminen tai tiedonkäsittelyn helpottaminen voi vaatia muutos ala- ja isoiksi kirjaimiksi. Tässä blogipostissa kerromme, kuinka tämän voi tehdä Arduinolla.

## Miten tehdä
Käytä toLower() funktiota muuntaaksesi tekstin pieniksi kirjaimiksi. Tässä on yksinkertainen esimerkki:
```Arduino
String teksti = "Terve vaan kaikille!";
String muunnettuTeksti = teksti.toLowerCase(); //muuttaa tekstin "terve vaan kaikille!"
```

## Syventyvä katsaus
toLower() funktio käyttää ASCII-taulukkoa muuttaakseen isot kirjaimet pieniksi. ASCII-taulukko on erityinen koodaussysteemi, joka määrittää jokaiselle kirjaimelle ja symbolille numerokoodin. Näin tietokoneet voivat tunnistaa ja käsitellä tekstejä.
toLower() funktio käyttää tätä numerokoodia ja sen perusteella muuttaa isot kirjaimet pieniksi. Esimerkiksi iso kirjain "A" vastaa numerokoodia 65 ja sen pieni vastine "a" on numerokoodilla 97.
Käytämme myös for-silmukkaa ja if-lausekkeita, jotta voimme käydä läpi jokaisen kirjaimen tekstin sisällä ja muuttaa sen tarvittaessa. Tässä on esimerkki hieman monimutkaisemmasta koodista:
```Arduino
String teksti = "Olen oppimassa Arduino-ohjelmointia!";
String muunnettuTeksti = "";

//Käydään läpi jokainen kirjain tekstissä
for (int i = 0; i < teksti.length(); i++) {
  //Otetaan yksittäinen kirjain muuttujaan
  char kirjain = teksti.charAt(i);
  
  //Tarkistetaan, onko kirjain iso ja muutetaan se pieneksi
  if (kirjain >= 'A' && kirjain <= 'Z') {
    kirjain = kirjain + 32;
  }
  
  //Lisätään kirjain muunnettuun tekstiin
  muunnettuTeksti = muunnettuTeksti + kirjain;
}
```

## Katso myös
- [ASCII-taulukko Arduino:ssa](https://www.arduino.cc/reference/en/language/variables/data-types/asciichart/)
- [toLower() funktion referenssi](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/tolower/)
- [for-silmukan käyttö Arduino:ssa](https://www.arduino.cc/reference/en/language/structure/control-structure/for/)
- [if-lausekkeen käyttö Arduino:ssa](https://www.arduino.cc/reference/en/language/structure/control-structure/if/)