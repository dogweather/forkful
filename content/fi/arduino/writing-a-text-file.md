---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Text-tiedostojen kirjoittaminen Arduino-ohjelmoinnissa voi olla hyödyllistä jos haluat tallentaa dataa laitteestasi tai näyttää tekstinäyttöön tietoja.

## Miten

Käytä komentoa "File.write" tallentaaksesi tekstiä haluamaasi tiedostoon. Esimerkiksi:

```Arduino
File tiedosto = SD.open("tiedosto.txt", FILE_WRITE);
if(tiedosto) {
  tiedosto.write("Tämä on tekstiä!");
  tiedosto.close();
}
```

## Syvempi sukellus

Voit myös käyttää "print" komentoa muuntaaksesi muuttujan arvon merkkijonoksi ja tallentaa sen tiedostoon. Esimerkiksi:

```Arduino
int luku = 10;
File tiedosto = SD.open("tiedosto.txt", FILE_WRITE);
if(tiedosto) {
  tiedosto.print("Luku on: ");
  tiedosto.print(luku);
  tiedosto.close();
}
```

## Katso myös
- "Kirjoita tekstiä Arduino:ssa" (https://www.arduino.cc/en/Reference/ArduinoSDWrite)
- "Merkkijonon tulostaminen Arduino:ssa" (https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)