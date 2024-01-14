---
title:                "Arduino: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi

Oletko koskaan törmännyt tarpeeseen yhdistää merkkijonoja Arduino-koodissa? Se voi tuntua yksinkertaiselta tehtävältä, mutta joskus se voi olla erittäin hyödyllistä, esimerkiksi kun halutaan luoda selkeämpiä ja monimutkaisempia tulosteita tai lukea useita erilaisia käyttäjän syötteitä.

# Miten tehdä

Jotta voit yhdistää merkkijonoja Arduino-koodissa, tarvitset kaksi asiaa: merkkijonoja ja yhdistämisoperaattorin (+). Merkkijonot voi määritellä tekstiksi tai numeroiksi. Alla on esimerkkejä merkkijonojen yhdistämisestä sekä lähtötekstistä, jonka pitäisi näkyä sarjamonitorissa.

```arduino
String nimi = "Liisa";
int ika = 25;

Serial.println("Minun nimeni on " + nimi + " ja olen " + ika + " vuotta vanha.");
```

*Tulostus: Minun nimeni on Liisa ja olen 25 vuotta vanha.*

Yhdistäminen toimii myös silloin, kun yhdistetään merkkijono ja muuttuja:

```arduino
int a = 5;
int b = 7;

Serial.println("Laskun tulos on " + a + b + ".");
```

*Tulostus: Laskun tulos on 12.*

# Syvällinen sukellus

Kun yhdistät merkkijonoja Arduino-koodissa, on tärkeää kiinnittää huomiota datan tyyppeihin. Yhdistämisen yhteydessä muuttujat muunnetaan automaattisesti merkkijonoiksi. Jos yhdistät esimerkiksi kaksi numeroarvoa, ne yhdistyvät matemaattisesti eikä tekstinä.

Lisäksi on tärkeää huomata, että yhdistetäänkö merkkijonoja muistissa oleviin muuttujiin vai luodaanko uusi merkkijono. Jos yhdistetään muuttujia, ne päivitetään myös muistissa olevien muuttujien arvoihin, kun taas uuden merkkijonon luominen ei vaikuta alkuperäisiin muuttujiin.

Kun yhdistät merkkijonoja, voi myös olla ongelmallista se, että tulokseen lisätään vahingossa ylimääräisiä välilyöntejä tai muita erikoismerkkejä. Tästä syystä on tärkeää olla tarkkana, minkä tyyppinen data yhdistetään ja miten.

# Katso myös

- String-luokan dokumentaatio [Arduino-verkkosivuilla](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- Stack Overflown [keskustelu](https://stackoverflow.com/questions/23765894/concatenating-numbers-and-strings-in-arduino) yhdistämisen hyödyntämisestä Arduinossa