---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:54.175094-07:00
description: "Standardivirheen (stderr) kirjoittaminen Arduinon ohjelmoinnissa tarkoittaa\
  \ virheilmoitusten ja diagnostiikkatietojen ohjaamista erilliselle kanavalle,\u2026"
lastmod: '2024-03-13T22:44:56.839789-06:00'
model: gpt-4-0125-preview
summary: "Standardivirheen (stderr) kirjoittaminen Arduinon ohjelmoinnissa tarkoittaa\
  \ virheilmoitusten ja diagnostiikkatietojen ohjaamista erilliselle kanavalle,\u2026"
title: Kirjoittaminen standardivirheeseen
---

{{< edit_this_page >}}

## Mikä & Miksi?

Standardivirheen (stderr) kirjoittaminen Arduinon ohjelmoinnissa tarkoittaa virheilmoitusten ja diagnostiikkatietojen ohjaamista erilliselle kanavalle, varmistaen, että ne eivät sekoitu standarditulosteeseen (stdout). Ohjelmoijat tekevät näin erottellakseen normaalit ohjelman tulosteet virheilmoituksista, mikä tekee vianmäärityksestä ja lokianalyysistä suoraviivaisempaa.

## Kuinka:

Arduino ei natiivisti erota standarditulostetta ja standardivirhettä kuten tavalliset tietokonejärjestelmät tekevät. Sekä `Serial.print()` että `Serial.println()` -metodit kirjoittavat samaan sarjatulosteeseen, jota tyypillisesti katsellaan Arduino IDE:n sarjamonitorista. Voimme kuitenkin emuloida kirjoittamista stderr:iin erityisesti muotoilemalla virheilmoituksia tai ohjaamalla ne vaihtoehtoiseen tulosteeseen, kuten tiedostoon SD-kortilla tai verkkoyhteyden kautta.

Stderr:n emuloimiseksi voit etuliittää virheilmoitukset tunnisteella kuten "VIRHE:" erottaaksesi ne sarjamonitorissa:

```cpp
void setup() {
  Serial.begin(9600); // Alusta sarjaviestintä 9600 bittinopeudella
}

void loop() {
  int tulos = jokuFunktio();
  if (tulos == -1) {
    // Emuloidaan stderr:iä etuliittämällä virheilmoitus
    Serial.println("VIRHE: Funktion suoritus epäonnistui.");
  } else {
    Serial.println("Funktio suoritettiin onnistuneesti.");
  }
  delay(1000); // Odota sekunti ennen silmukan uudelleenaloitusta
}

int jokuFunktio() {
  // Esimerkkifunktio, joka palauttaa -1 virheessä
  return -1;
}
```

Esimerkkituloste Arduino IDE:n sarjamonitorissa saattaisi näyttää tältä:

```
VIRHE: Funktion suoritus epäonnistui.
```

Projekteissa, jotka vaativat monimutkaisempaa lähestymistapaa, mukaan lukien kirjoittaminen eri fyysisiin tulosteisiin, kolmannen osapuolen kirjastojen tai lisälaitteiston käyttö saattaa olla tarpeen. Esimerkiksi virheilmoitusten kirjaaminen SD-kortille vaatii `SD`-kirjaston:

```cpp
#include <SPI.h>
#include <SD.h>

File tiedostoni;

void setup() {
  Serial.begin(9600);
  if (!SD.begin()) {
    Serial.println("VIRHE: SD-kortin alustus epäonnistui!");
    return;
  }
  
  tiedostoni = SD.open("error.log", FILE_WRITE);
  if (tiedostoni) {
    tiedostoni.println("VIRHE: Funktion suoritus epäonnistui.");
    tiedostoni.close(); // Varmista, että suljet tiedoston sisällön tallentamiseksi
  } else {
    Serial.println("VIRHE: error.log-tiedoston avaaminen epäonnistui!");
  }
}

void loop() {
  // Pääkoodisi menisi tähän
}
```

Tällä lähestymistavalla fyysisesti erotat normaalit ohjelman tulosteet ja virheilmoitukset ohjaamalla jälkimmäiset `error.log`-tiedostoon SD-kortilla, mahdollistaen jälkikäteisanalyysit ilman, että pääkanava sotkeentuu.
