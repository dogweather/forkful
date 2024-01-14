---
title:                "Arduino: Luodaan väliaikainen tiedosto."
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi luoda väliaikainen tiedosto Arduinolla?

Monissa projekteissa saatetaan törmätä tilanteeseen, jossa tarvitsee tallentaa väliaikaista tietoa, mutta ei haluta tai tarvitse käyttää pysyvää tallennusmuistia. Tässä tapauksessa väliaikaisen tiedoston luominen Arduinoon voi olla hyödyllistä.

## Miten tehdä väliaikainen tiedosto Arduinolla?

Väliaikaisen tiedoston luominen Arduinossa on helppoa käyttämällä `File`-kirjastoa. Ensimmäiseksi tulee alustaa tiedostonhallinta käyttäen `SD.begin()`-funktiota ja määritellä mikä SD-kortin liitäntä käytetään (esimerkiksi `SD.begin(4)` jos SD-kortti on kytketty liitäntään 4). Sen jälkeen voidaan luoda uusi `File`-objekti, johon voidaan tallentaa haluttu data.

```Arduino
#include <SPI.h>
#include <SD.h>

File tmpFile;

void setup() {
  pinMode(4, OUTPUT); // määritä liitäntäkortti
  if (!SD.begin(4)) {
    Serial.println("Virhe: SD-kortin liitäntä epäonnistui!");
    return;
  }
  tmpFile = SD.open("tmp.txt", FILE_WRITE); // luo tiedosto ja avaa se kirjoittamista varten
  if (tmpFile) { // tarkista onko tiedosto avattu onnistuneesti
    tmpFile.println("Tässä on väliaikainen tiedosto!"); // kirjoita data tiedostoon
    tmpFile.close(); // sulje tiedosto
    Serial.println("Tiedosto luotu ja tallennettu!");
  }
  else {
    Serial.println("Virhe: tiedoston luonti epäonnistui!");
  }
}

void loop() {
  // tee jotain muuta
}
```

Kun koodi ajetaan, se luo uuden tiedoston nimeltä "tmp.txt" SD-kortille ja tallentaa siihen tekstin "Tässä on väliaikainen tiedosto!". Sen jälkeen tiedosto suljetaan ja tulostetaan viesti onnistuneesta luomisesta.

## Syvällisempi sukellus väliaikaisen tiedoston luomiseen Arduinolla

Väliaikaisen tiedoston luominen voidaan tehdä myös ohittamalla tiedoston nimi ja käyttämällä `SD.open()`-funktiota ilman parametreja, jolloin tiedostolle luodaan automaattisesti uniikki nimi. Tämä voi olla kätevää, jos kyseessä on useita väliaikaisia tiedostoja tai tiedostolle ei ole tarvetta antaa erityistä nimeä.

Lisäksi väliaikaisen tiedoston voi luoda myös `File`-objektilla ilman SD-korttia käyttämällä `tmpFile = File();` ja tallentaa siihen dataa kuten tavalliseen tiedostoon. Tällöin tiedosto kuitenkin häviää kun Arduino sammuu, joten tällaista käyttöä varten on suositeltavaa käyttää SD-korttia.

# Katso myös

- [Arduino File-kirjaston dokumentaatio (englanniksi)](https://www.arduino.cc/en/Reference/SD)
- [SD-kortin valitseminen ja alustaminen Arduinolla (englanniksi)](https://randomnerdtutorials.com/complete-guide-esp32-sd-card-memory/)
- [File-olio Arduino-koodauksessa (englanniksi)](https://www.arduino.cc/en/Tutorial/FileWrite)