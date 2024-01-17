---
title:                "Tietokoneohjelmointi: Merkkijonojen yhdistäminen"
html_title:           "Arduino: Tietokoneohjelmointi: Merkkijonojen yhdistäminen"
simple_title:         "Tietokoneohjelmointi: Merkkijonojen yhdistäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Miksi ohjelmoijat yhdistävät merkkijonoja? Merkkijonojen yhdistäminen tarkoittaa kahden tai useamman merkkijonon yhdistämistä yhdeksi merkkijonoksi. Tämä on hyödyllistä, kun halutaan luoda uusia ja monipuolisempia tietoja, kuten esimerkiksi tekstimuotoisia viestejä, otsikoita tai kokonaisia lauseita.

# Miten:

Arduino-ohjelmointiympäristössä merkkijonojen yhdistäminen tehdään käyttämällä "+" -operaattoria. Tämä yhdistää kaksi merkkijonoa yhdeksi. Alla on esimerkki, miten voit yhdistää "Hei" ja "maailma" yhdeksi merkkijonoksi "Hei maailma".

```Arduino
String ensimmainen = "Hei"
String toinen = "maailma"

String yhdistetty = ensimmainen + " " + toinen;

Serial.println(yhdistetty); //tulostaa "Hei maailma" sarjaliikenneportille
```

# Syvällinen sukellus:

Merkkijonojen yhdistäminen on ollut käytössä ohjelmoinnissa jo pitkään ja sitä käytetään yleisesti kaikentyyppisissä ohjelmointikielissä. Lisäksi, voi myös käyttää "String.format()" -funktiota yhdistelemään merkkijonoja monimutkaisilla tavoilla, mikäli halutaan esimerkiksi sisällyttää muuttujia osaksi yhdistettyä merkkijonoa. Siinä tavoin muodostuu kompleksisia viestejä, kuten laskuja tai muistiinpanoja.

# Katso myös:

[Koko ohjelmointiopas Arduino:lle](https://www.arduino.cc/en/Guide/Introduction)

[Stack Overflow -kysymys merkkijonojen yhdistämisestä Arduinossa](https://stackoverflow.com/questions/30040485/concatenate-string-and-integer-arduino)