---
title:                "Kirjoittaminen standardivirheille"
html_title:           "Arduino: Kirjoittaminen standardivirheille"
simple_title:         "Kirjoittaminen standardivirheille"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Kun ohjelmoijat puhuvat standardivirheestä, he viittaavat tulostamaan virhesanomia ohjelmoinnin aikana. Tämä auttaa ohjelmoijaa tunnistamaan ja korjaamaan mahdollisia virheitä ohjelman suorituksen aikana.

## Miten:
Esimerkki koodi:
```
void setup() {
  Serial.begin(9600);
  Serial.println("Tämä on standardivirhe");
  Serial.print("Koodin rivi: ");
  Serial.println(__LINE__);
}

void loop() {
  //Kirjoita koodi tänne
}
```

Esimerkki tuloste:
```
Tämä on standardivirhe
Koodin rivi: 4
```

## Syväsukellus:
Standardivirheen kirjoittaminen juontaa juurensa C-kieleen, jossa se oli tapa ilmoittaa virheistä ohjelman suorituksen aikana. Tänä päivänä on myös muita vaihtoehtoja, kuten käyttää debuggaukseen soveltuvia kirjastoja tai jättää virheet kokonaan huomiotta.

## Katso myös:
- [Arduino Serial Communication](https://www.arduino.cc/reference/en/language/functions/communication/serial/)