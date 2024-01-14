---
title:                "Arduino: Alimerkkijonojen erottaminen"
simple_title:         "Alimerkkijonojen erottaminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Arduino-ohjelmointi ei ole aina helppoa, mutta käyttämällä substringien louhimista voit helposti hallita merkkijonoja ja tehdä monimutkaisemmat tehtävät yksinkertaisiksi. Joten miksi et hyödyntäisi tätä hyödyllistä toimintoa?

## Kuinka

Substringien louhiminen Arduinossa on yksinkertaista ja nopeaa. Käytämme tätä kätevää toimintoa merkkijonoissa, kun meidän on esimerkiksi otettava tietty osa merkkijonosta ja käsiteltävä sitä erikseen.

```
Arduino String merkkijono = "Hei, olen Arduino!";
String osa1 = merkkijono.substring(1); // palauttaa "ei, olen Arduino!"
String osa2 = merkkijono.substring(4, 7); // palauttaa "olen"
```

## Syvemmälle

Substringien louhinta perustuu kahteen parametriin: aloituskohdan indeksiin ja mahdolliseen lopetuskohdan indeksiin. Ensimmäinen parametri on pakollinen ja se määrittää kohdan, josta haluat aloittaa louhimisen. Toinen parametri on valinnainen ja se määrittää kohdan, josta haluat lopettaa louhimisen. Jos toista parametria ei anneta, substring louhitaan loppuun saakka.

`substring()`-funktion käyttö tulee erittäin hyödylliseksi, kun haluat esimerkiksi ottaa osan käyttäjän syöttämästä merkkijonosta tai tarkistaa, onko tietty sana tai lause osa merkkijonoa.

## Katso myös

- [Arduino String.substring() reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Arduino String.charAt() blogikirjoitus](https://www.arduino.cc/en/Tutorial/StringCharAt)
- [Arduino perusteet -blogi](https://www.arduino.cc/en/Tutorial/Foundations)