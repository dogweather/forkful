---
title:                "Arduino: Lausekkeen pituuden löytäminen"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Miksi: Miksi on tärkeää löytää merkkijonon pituus?

Merkkijonon pituuden löytäminen on tärkeä osa ohjelmointia, sillä se auttaa meitä hallitsemaan ja muokkaamaan tekstipohjaista dataa. Esimerkiksi voi olla tarpeellista tietää, kuinka monta merkkiä tai sanaa mahtuu tiettyyn muuttujaan tai käyttöliittymään.

# Miten: Koodiesimerkkejä merkkijonon pituuden löytämiseen Arduino-ympäristössä

```arduino
String teksti = "Tervetuloa!";

// Lasketaan merkkijonon pituus ja tulostetaan se sarjaväylään
int pituus = teksti.length();
Serial.println(pituus);

// Voimme myös tulostaa vain osan merkkijonosta, esimerkiksi ensimmäiset 5 merkkiä
Serial.println(teksti.substring(0, 5));
```

Tuloste: 
```
11
Terve
```

# Syvempi sukellus: Tietoa merkkijonon pituuden löytämisestä

Merkkijonon pituus lasketaan oletuksena kaikkien merkkien mukaan, myös välilyönnit ja erikoismerkit lasketaan mukaan. On myös mahdollista käyttää muita metodeja, kuten ```.trim()``` ja ```.remove()``` joiden avulla voidaan muokata merkkijonoa ennen sen pituuden laskemista. Lisätietoa näistä ja muista hyödyllisistä string-metodeista löydät [Arduino String -dokumentaatiosta](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/).

# Katso myös

- [Merkkijonojen käyttö Arduino-ohjelmoinnissa - opetusvideo](https://www.youtube.com/watch?v=_JNE0jJyt54)
- [Arduino-ohjelmointikurssi - merkkijonot](https://www.arduino.cc/en/Tutorial/StringLength)
- [Miten ohjelmoida merkkijonoja Arduinolla](https://create.arduino.cc/projecthub/charlee/arduino-tutorial-how-to-use-strings-f88a95)