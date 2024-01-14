---
title:    "Arduino: Väliaikaisen tiedoston luominen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi luoda väliaikainen tiedosto Arduino-ohjelmoinnissa?

Arduinon ohjelmointi on jännittävä harrastus, joka tarjoaa lukemattomia mahdollisuuksia luovalle ajattelulle ja projektien toteuttamiselle. Väliaikaisten tiedostojen luominen on yksi tapa, jolla voit hyödyntää Arduinoa luodessasi monimutkaisempia ohjelmia. 

## Kuinka luoda väliaikainen tiedosto Arduino-ohjelmassa?

Luodaksesi väliaikaisen tiedoston Arduino-ohjelmassa, sinun täytyy käyttää SPIFFS-tiedostojärjestelmää. Tämä tiedostojärjestelmä antaa sinulle mahdollisuuden tallentaa tiedostot Arduinon sisäiselle Flash-muistille. Seuraa alla olevia vaiheita luodaksesi väliaikaisen tiedoston:

```
ArduinoOTA.onError([](ota_error_t error) {
  Serial.println(error);
});
```

Ensinnäkin sinun täytyy alustaa SPIFFS-tiedostojärjestelmä alussa ```setup()```-funktiossa. Voit tehdä tämän seuraavasti: 

```
void setup() {
  if(!SPIFFS.begin(true)) {
    Serial.println("An error has occurred while mounting SPIFFS");
    return;
  }
}
```

Seuraavaksi voit luoda uuden tiedoston avaamalla ```File```-objektin ja käyttämällä metodia ```open()```. Tämä metodi ottaa kaksi parametria: tiedoston nimen ja avauskoodin. 

```
File tempFile = SPIFFS.open("temp.txt", "w+");
if(!tempFile) {
  Serial.println("There was an error creating the temp file");
  return;
}
```

Nyt voit kirjoittaa tiedostoon käyttämällä tiedostoon liitettyä ```print()```-metodia.

```
tempFile.print("Hello world!");
```

Voit myös lukea tiedostosta käyttämällä ```read()```-metodia.

```
String fileContent = tempFile.readString();
Serial.println(fileContent);
```

Muista sulkea tiedosto käytön jälkeen käyttämällä ```close()```-metodia.

```
tempFile.close();
```

## Syvällisempi sukellus väliaikaisen tiedoston luomiseen

Tiedostojen luominen ja hallitseminen on välttämätöntä monimutkaisempien Arduino-ohjelmien luomisessa. Luomalla väliaikainen tiedosto, voit helposti tallentaa ja käsitellä tietoja, joita tarvitset ohjelmassasi. Voit myös luoda useita väliaikaisia tiedostoja ja käyttää niitä eri tarkoituksiin. Tämä antaa sinulle enemmän hallintaa ohjelmasi suorituskyvyssä ja mahdollistaa monimutkaisempien toimintojen toteuttamisen.

## Katso myös

- [Arduino SPIFFS-tiedostojärjestelmän dokumentaatio](https://arduino-esp8266.readthedocs.io/en/latest/filesystem.html)
- [Väliaikaisten tiedostojen luominen C++:lla](https://www.programiz.com/cpp-programming/files)
- [SPIFFS-tiedostojen lukeminen ja kirjoittaminen Arduino-ohjelmassa](https://randomnerdtutorials.com/esp32-publishing-dht11-dht22-readings-to-sqlite-database/)