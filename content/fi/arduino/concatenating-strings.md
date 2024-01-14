---
title:                "Arduino: Merkkijonojen yhdistäminen"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Miksi

Miksi haluaisit yhdistellä merkkijonoja ohjelmoinnissa? Stringien yhdistäminen on kätevä tapa luoda dynaamisia tekstisisältöjä, kuten käyttäjän syöttämistä tietoja tai sensorien lukemia. Tämä lisää monipuolisuutta Arduino-projekteihin ja mahdollistaa interaktiivisemman kokemuksen.

# Kuinka käytän

```Arduino
String teksti = "Hei ";
String nimi = "Arduino";
String tervehdys = teksti + nimi;
Serial.print(tervehdys);
```

Tämä koodi luo uuden String-objektin nimeltä "tervehdys", joka yhdistää kaksi muuta stringiä, "Hei" ja "Arduino". Tämän jälkeen tulostetaan tervehdys sarjaportille ja tulokseksi saadaan "Hei Arduino". Voit myös yhdistää useampia stringejä yhteen, käyttämällä enemmän plus-merkkejä, kuten "String lause = otsikko + teksti + lisäys;".

# Syvällinen tutkiskelu

Stringien yhdistäminen Arduino-ohjelmoinnissa voi vaikuttaa yksinkertaiselta, mutta oikeastaan kyseessä on hieman monimutkaisempi prosessi. Kun yhdistät stringejä, Arduino luo uuden merkkijono-objektin muistissa ja kopioi yhdistetyt merkkijonot siihen. Tämän vuoksi suurten stringien yhdistäminen voi vaikuttaa hitaammalta ja käyttää enemmän muistia. On myös tärkeää muistaa, että stringien yhdistäminen voi aiheuttaa katkoja kodin suorittamisessa, joten kannattaa käyttää sitä vain tarvittaessa.

# Katso myös

Voit lukea lisätietoja stringien yhdistämisestä Arduino-ohjelmoinnissa seuraavista lähteistä:

- [Arduino String-ohjeet](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Yksityiskohtainen opas stringien käytöstä Arduinossa](https://www.electroschematics.com/construct-string-arduino/)
- [Video-opas stringien manipuloinnista Arduinolla](https://www.youtube.com/watch?v=HNa_UXFGYw4)