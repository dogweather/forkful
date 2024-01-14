---
title:                "Arduino: Tarkistetaan, onko kansio olemassa."
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Miksi tarkistaa kansio olemassaolo Arduino-ohjelmoinnissa?

Kannattaa tarkistaa, onko kansio olemassa, jotta voidaan varmistaa ohjelman käyttöliittymän ja eri moduulien ongelmaton toiminta. 

## Kuinka tarkistaa kansio olemassaolo Arduino-ohjelmoinnissa

Arduino-ohjelmointiympäristössä on käytössä `File` -kirjasto, joka sisältää joukon funktioita tiedostojen ja kansioiden käsittelyyn. `File` -kirjaston `exists()` -funktio mahdollistaa meidän tarkistaa, onko kansio olemassa vai ei. Seuraavassa esimerkissä luodaan kansio nimeltä "uusi kansio" ja tarkistetaan sen olemassaolo:

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  if (SD.exists("uusi kansio")) {
    Serial.println("Kansio on olemassa!");
  } else {
    Serial.println("Kansiota ei ole olemassa.");
  }
  delay(1000);
}
```

Ohjelma tulostaa joko "Kansio on olemassa!" tai "Kansiota ei ole olemassa." riippuen siitä, löytyykö kansio nimeltä "uusi kansio" SD-kortilta vai ei.

## Syvällisempi tarkastelu kansio olemassaolon tarkistamisesta

`exists()` -funktio tarkistaa, onko kansio olemassa juuri sillä hetkellä, kun sitä kutsutaan. Ohjelmakoodissa voi kuitenkin ilmetä tilanteita, joissa kansio olemassaolon tarkistamista on tarpeen toistaa useammin esimerkiksi erilaisten ehtojen perusteella. Tällöin on suositeltavaa tallentaa `File`-olio omaan muuttujaansa ja käyttää `File::exists()` -funktiota sen avulla. `File`-olio pysyy voimassa niin kauan kuin ohjelmassa pysytään saman sisäisen tai ulkoisen muistin alueella. Tarkastellaan seuraavassa esimerkissä tilannetta, jossa kansio olemassaolon tarkistaminen toistetaan:

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  Serial.begin(9600);
}

void loop() {
  File kansio = SD.open("uusi kansio");
  if (!kansio) {
    Serial.println("Kansiota ei ole olemassa!");
  } else {
    Serial.println("Kansio on nyt olemassa!");
  }
  kansio.close();
  delay(1000);
}
```

Tässä tapauksessa `open()` -funktio avaa kansion ja antaa sen osoitteen `kansio` nimiselle `File`-oliolle. Sen jälkeen `exists()` -funktiota voidaan käyttää `kansio`-muuttujan avulla. Lopuksi vielä `kansio` suljetaan `close()` -funktiolla.

## Katso myös

- [SD-kortin ohjelmointi Arduino-ohjelmistolla](https://www.arduino.cc/en/Reference/SD/)
- [File-kirjaston dokumentaatio](https://www.arduino.cc/en/Reference/File/)