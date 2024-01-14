---
title:    "Arduino: Merkkijonojen yhdistäminen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi

Joskus Arduino-ohjelmoijille tulee tarve yhdistää useita merkkijonoja yhdeksi. Tämä voi olla hyödyllistä esimerkiksi näytön viestien luomisessa tai sensordatan tallentamisessa.

## Miten

Arduinolla merkkijonojen yhdistäminen tapahtuu käyttäen `+`-operaattoria. Alla on esimerkki, jossa kaksi merkkijonoa "Hello" ja "world" yhdistetään ja tulostetaan sarjamonitoriin:

```Arduino
String s1 = "Hello";
String s2 = "world";
String s3 = s1 + " " + s2;
Serial.println(s3); // Tulostaa "Hello world"
```

## Syvempi sukellus

On tärkeää huomata, että Arduinolla merkkijonat tallennetaan `String`-muuttujina, eikä niitä voi käsitellä kuten tavallisia merkkijonoja esimerkiksi C++:ssa. Tästä syystä merkkijonojen yhdistäminen `+`-operaattorilla voi aiheuttaa ylimääräistä muistinkäyttöä ja hidastaa ohjelman suoritusta.

Hyvä käytäntö onkin käyttää `String.concat()`-funktiota, joka yhdistää merkkijonot suoraan alkuperäiseen `String`-muuttujaan, vähentäen näin muistinkäyttöä ja parantaen ohjelman suorituskykyä. Alla on esimerkki:

```Arduino
String s1 = "Hello";
String s2 = "world";
s1.concat(" ");
s1.concat(s2);
Serial.println(s1); // Tulostaa "Hello world"
```

On myös mahdollista käyttää `char`-taulukoita merkkijonojen yhdistämiseen tavallisilla merkkijonafunktioilla, kuten `strcpy()` tai `strcat()`. Tämä kuitenkin vaatii hieman enemmän osaamista ja työtä verrattuna `String`-muuttujien käyttämiseen.

## Katso myös

- [Arduino - Strings](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutorial: How to concatenate strings in Arduino](https://www.cybersolutionsrochester.com/blog/tutorials/tutorial-how-to-concatenate-strings-in-arduino/)