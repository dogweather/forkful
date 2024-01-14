---
title:    "Arduino: Väliaikaisen tiedoston luominen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Arduino-ohjelmointi: Tietoa väliaikaisten tiedostojen luomisesta

## Miksi
 Miksi joku haluaisi luoda väliaikaisen tiedoston Arduino-ohjelmassa? Väliaikaisten tiedostojen luominen voi olla hyödyllistä silloin, kun haluat tallentaa väliaikaisesti tietoa, jota ei tarvita pysyvästi. Tämä voi olla hyödyllistä esimerkiksi tilanteissa, joissa tarvitset tallennustilaa, mutta et halua käyttää pysyvää muistia.

## Kuinka
Luodaksesi väliaikaisen tiedoston Arduino-ohjelmassa, sinun on ensin varattava tarvittava muistialue ja määritettävä sen koko. Tämän jälkeen voit luoda tiedoston käyttämällä "create()"-toimintoa. Alla on esimerkki koodista ja siihen liittyvä tulostus:

```Arduino
#include <SPI.h>
#include <SD.h>

File tempFile; //määritetään muuttuja tiedostolle

void setup() {
  Serial.begin(9600);
  SD.begin();
  tempFile = SD.open("temp.txt", FILE_WRITE); //luodaan tiedosto SD-kortille
  if (tempFile) { //jos tiedosto löytyy
    Serial.println("Väliaikainen tiedosto luotu!");
  } else { //muussa tapauksessa
    Serial.println("Tiedostoa ei voitu luoda.");
  }
}

void loop() {
  //tässä voit kirjoittaa tiedostoon tarvittavaa tietoa
  tempFile.print("Tämä on väliaikainen tiedosto.");
  tempFile.close(); //suljetaan tiedosto kun tallennus on valmis
  Serial.println("Tiedot tallennettu.");
  delay(1000); //viiveen vuoksi
}
```

Tulostus:

```
Väliaikainen tiedosto luotu!
Tiedot tallennettu.
```

## Syvällisempi tarkastelu
Väliaikaisten tiedostojen luominen voi olla hyödyllistä erilaisissa projekteissa, joissa tarvitaan väliaikaista tallennustilaa. Tiedostoja voidaan luoda myös tietyn aikarajan jälkeen poistettavaksi, jolloin ne voidaan käyttää esimerkiksi laskennallisena apuna. On myös tärkeää huomata, että väliaikaisia tiedostoja ei tule käyttää pysyvänä tallennusmuotona, sillä ne poistuvat laitteesta käytön jälkeen.

### Huomioitavia asioita:
- Varmista, että sinulla on riittävästi muistia väliaikaisen tiedoston luomiseen.
- Muista sulkea tiedosto kun tallennus on valmis, muuten siitä voi tulla vioittunut tai ylikirjoitettu.
- Vältä käyttämästä väliaikaisia tiedostoja pysyvänä tallennusmuotona.

## Katso myös
- ["SD.h" kirjasto (virallinen dokumentaatio)](https://www.arduino.cc/en/Reference/SD)
- ["File" kirjasto (virallinen dokumentaatio)](https://www.arduino.cc/en/Reference/File)