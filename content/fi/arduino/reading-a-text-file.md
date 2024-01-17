---
title:                "Tiedoston lukeminen"
html_title:           "Arduino: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Lukeminen tekstitiedostosta tarkoittaa tekstin lukemista tietokoneelle tallennetusta tiedostosta. Ohjelmoijat tekevät tätä esimerkiksi tiedon tallentamiseksi ja käsittelyksi.

# Kuinka:
Esimerkiksi tiedostossa "data.txt" on teksti "Hello World!", haluamme lukea tämän tekstin ja tulostaa sen sarjaporttiin.

```Arduino
File tiedosto = SD.open("data.txt", FILE_READ);
if (tiedosto) {
  while (tiedosto.available()) {
    Serial.println(tiedosto.read());
  }
  tiedosto.close();
}
```
Tulostamme sarjaporttiin:

```
72
101
108
108
111
32
87
111
114
108
100
33
```

# Syvemmälle:
Tiedostojen lukeminen on ollut tärkeä osa ohjelmointia jo pitkään. Nykyään on olemassa myös muita tapoja lukea tiedostoja, kuten hakukoneita ja web scraping -työkaluja.

# Katso myös:
- https://www.arduino.cc/en/Reference/SD
- https://www.arduino.cc/en/Tutorial/ReadASCIIString