---
title:    "Arduino: Merkkijonojen yhdistäminen"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

On monia tapoja hyödyntää merkkijonojen yhdistämistä Arduinon ohjelmoinnissa. Se voi olla hyödyllistä esimerkiksi tekstin muodostamisessa, tiedon tallentamisessa tai käyttäjän kanssa vuorovaikutuksessa.

## Kuinka tehdä

Merkkijonojen yhdistäminen Arduinolla on yksinkertaista ja se voidaan tehdä useilla eri tavoilla. Yksi tapa on käyttää "concat()" funktiota, joka yhdistää kaksi merkkijonoa yhdeksi. Tässä on esimerkki koodista ja sen tulosteesta:

```Arduino
String etunimi = "Maija";
String sukunimi = "Meikäläinen";
String nimi = etunimi.concat(" ", sukunimi);
Serial.println(nimi);
```

Tulostus: Maija Meikäläinen

Toinen tapa on käyttää "+" operaattoria, joka yhdistää kaksi merkkijonoa yhdeksi. Tässä on esimerkki koodista ja sen tulosteesta:

```Arduino
String kaupunki = "Helsinki";
String maa = "Suomi";
String sijainti = kaupunki + ", " + maa;
Serial.println(sijainti);
```

Tulostus: Helsinki, Suomi

## Syvällisempi tarkastelu

Merkkijonojen yhdistäminen on hyödyllinen taito Arduinon ohjelmoinnissa. Yksi tärkeä seikka on ymmärtää, että merkkijonat ovat muuttumattomia Arduinon String-kirjaston avulla. Tämä tarkoittaa, että aina, kun merkkijonoa muokataan, uusi merkkijono luodaan. Siksi on tärkeää varata tarpeeksi muistitilaa, jotta ohjelma ei kaadu.

On myös tärkeää huomata, että merkkijonien yhdistäminen voi hidastaa ohjelman suorituskykyä. Jos sinulla on paljon merkkijonoja, jotka täytyy yhdistää yhteen, on parempi käyttää "concat()" funktiota, koska se on tehokkaampi kuin "+" operaattori.

Lisäksi on hyvä muistaa, että merkkijonojen yhdistäminen voi aiheuttaa ylimääräisiä välilyöntejä, jos niitä ei huolellisesti sijoiteta. On tärkeää olla tarkkaavainen, jotta ohjelmasi toimii halutulla tavalla.

## Katso myös

- [String-kirjaston dokumentaatio](https://www.arduino.cc/reference/en/language/variables/data-types/stringclass/)
- [Merkkijonojen manipulointi Arduinolla](https://www.arduino.cc/en/Tutorial/StringConstructors)
- [Merkkijonojen yhdistämisen tehokkuus Arduinolla](https://www.geekstips.com/arduino-string-concatenation/)