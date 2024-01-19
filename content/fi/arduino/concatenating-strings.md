---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonojen yhdistäminen, eli konkatenointi, on tapa yhdistää kaksi tai useampia merkkijonoja toisiinsa. Ohjelmoijat tekevät sen muokatakseen tai esittääkseen dataa haluamallaan tavalla.

## Kuinka tehdä:

Arduino-koodissa on lukuisia tapoja yhdistää merkkijonoja. Poimimme muutaman yksinkertaisen esimerkin.

```Arduino
String stringOne = "Hei ";
String stringTwo = "Maailma!";
String greeting = stringOne + stringTwo;
Serial.println(greeting);  // Tulostaa: "Hei Maailma!"
```

Voit yhdistää string-luokan avulla käyttämällä '+'-operaattoria, kuten yllä, tai voit käyttää concat()-metodia.

```Arduino
String stringOne = "Hei ";
String stringTwo = "Maailma!";
stringOne.concat(stringTwo);
Serial.println(stringOne);  // Tulostaa: "Hei Maailma!"
```

## Syvä sukellus

**Historiallinen konteksti:** Konkatenaatio on ollut tärkeä osa ohjelmointia sen alkuajoista lähtien. Se on varhainen tapa manipuloida tekstiä eri ohjelmissa.

**Vaihtoehdot:** Arduino tarjoaa myös sprintf()-toimintoa, joka voidaan olla tehokkaampi muistinkäytön kannalta isommissa projekteissa.

```Arduino
char buffer[50];
char *s = "Hei ";
char *t = "Maailma!";
sprintf(buffer, "%s%s", s, t);
Serial.println(buffer);  // Tulostaa: "Hei Maailma!"
```

**Toteutuksen yksityiskohdat:** Muista, että kaikki merkkijonot Arduino-koodissa käyttävät muistia. Pyri pitämään merkkijonot ja niiden käyttö minimaalisena muistin säästämiseksi.

## Katso myös

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/) - Arduinon merkkijonoluokan dokumentaatio ja lisätiedot.
- [Concatenation on Wikipedia](https://en.wikipedia.org/wiki/Concatenation) - Lisätietoa merkkijonojen yhdistämisestä ja sen historia.