---
date: 2024-01-26 00:59:29.100907-07:00
description: "\"Lokin pit\xE4minen\" tarkoittaa tapahtumien, transaktioiden tai toimintojen\
  \ kirjaamista, jotka tapahtuvat ajan kuluessa j\xE4rjestelm\xE4ss\xE4. Ohjelmoijat\
  \ k\xE4ytt\xE4v\xE4t\u2026"
lastmod: '2024-03-13T22:44:56.830178-06:00'
model: gpt-4-1106-preview
summary: "\"Lokin pit\xE4minen\" tarkoittaa tapahtumien, transaktioiden tai toimintojen\
  \ kirjaamista, jotka tapahtuvat ajan kuluessa j\xE4rjestelm\xE4ss\xE4."
title: Lokitus
weight: 17
---

## Kuinka:
Arduino ei sisällä sisäänrakennettua lokikirjaston toimintoa, kuten jotkin muut ympäristöt, mutta voit toteuttaa peruslokin Serial-konsoliin vähällä vaivalla. Tässä on nopea esimerkki, jolla pääset alkuun:

```arduino
void setup() {
  // Aloita sarjaviestintä annetulla baudinopeudella
  Serial.begin(9600);

  // Odota sarjaportin yhdistämistä - tarpeellista vain joillakin levyillä
  while (!Serial) {
    ; // odota, että sarjaportti yhdistää. Tarvitaan natiivia USB:tä varten
  }

  // Kirjaa informatiivinen viesti, joka ilmoittaa, että asetukset ovat valmiit
  Serial.println("Asetukset valmiina!");
}

void loop() {
  // Yksinkertainen lokikirjanpitäjä, joka tulostaa käyttöajan joka sekunti
  static unsigned long lastLogTime = 0;
  unsigned long currentMillis = millis();

  if (currentMillis - lastLogTime >= 1000) {
    lastLogTime = currentMillis;
    Serial.print("Käyttöaika (ms): ");
    Serial.println(currentMillis);

    // Tähän voit lisätä myös virhelokeja, varoituksia tai muuta tietoa.
  }
  
  // Muu ohjelmasi logiikka täällä...
}
```

Esimerkki Serial-konsolin tulosteesta:
```
Asetukset valmiina!
Käyttöaika (ms): 1000
Käyttöaika (ms): 2000
Käyttöaika (ms): 3000
...
```

## Syventävä tarkastelu:
Historiallisesti lokin kirjaaminen mikrokontrollereissa ei ollut yhtä yksinkertaista kuin täysiverisessä käyttöjärjestelmässä. Rajalliset resurssit tarkoittivat, että jokaisella tavulla oli merkitystä, ja kehittäjien oli oltava varovaisia, etteivät tukkisi järjestelmää. Lisäkykyisten levyjen ja Arduino-alustan helpottaessa prosessia, lokin kirjaamisesta on tullut helpommin saavutettavaa.

Vaikka edellä oleva koodi esittää lokin kirjaamista Serial-rajapinnan kautta, muita menetelmiä ovat kirjoittaminen SD-kortille, datan lähettäminen verkon yli etäpalvelimelle tai jopa tulostaminen pieneen LCD-näyttöön.

Lokijärjestelmän toteuttaminen tuo mukanaan harkittavia seikkoja, kuten kierron, vakavuusasteen (info, debug, varoitus, virhe) ja suorituskyvyn vaikutuksen. Arduinolla sinun on ehkä oltava tietoinen muistirajoitteista lokittaessa monimutkaisia tietorakenteita. Etälokittaessa on myös huolestuttava lähetettyjen lokien turvallisuudesta.

Sofistikoituneempia ratkaisuja, kuten laajasti hyväksytty lokistandardi Syslog, on olemassa Arduinon ulkopuolella, mutta voit integroida kolmansien osapuolien kirjastoja, jotka tarjoavat samankaltaista toiminnallisuutta eri tasoisin kompleksisuusin ja resurssivaatimuksin.

## Katso myös:
- [Arduino `Serial` viite](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [SD-kortille lokin kirjaaminen Arduinolla](https://www.arduino.cc/en/Tutorial/LibraryExamples/Datalogger)
- [SparkFunin Data Logging -kilpi](https://www.sparkfun.com/products/13712)
- [TinyWeb: Käytännön esimerkki etälokista Arduinolla](https://www.arduino.cc/en/Tutorial/WebClientRepeating)
