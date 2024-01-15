---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Java: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Yhdistäminen on tärkeä osa ohjelmointia, joka mahdollistaa merkkijonojen yhdistämisen yhdeksi kokonaisuudeksi. Tämä on hyödyllistä esimerkiksi tekstin muokkaamisessa ja tulostamisessa.

## Kuinka

```Java
String s1 = "Hello";
String s2 = "world";
String s3 = s1 + " " + s2;
System.out.println(s3);
```

Tämä koodiesimerkki yhdistää kaksi merkkijonoa "Hello" ja "world" yhdeksi merkkijonoksi "Hello world" käyttäen plus-merkkiä (+). Voit myös yhdistää useampia merkkijonoja samalla tavalla.

```Java
String s1 = "Java is ";
String s2 = "cool!";
String s3 = s1 + s2;
System.out.println(s3);
```

Tulostettu tulos on "Java is cool!". Lisäksi voit myös yhdistää muuttujia ja kirjaimia merkkijonoon.

```Java
String language = "Java";
String sentence = language + " is the best programming language!";
System.out.println(sentence);
```

Tulostettu tulos on "Java is the best programming language!".

## Syventävä tieto

Yhdistäminen ei rajoitu vain merkkijonoihin, vaan voit myös yhdistää muita tietotyyppejä, kuten numerot.

```Java
int num1 = 5;
int num2 = 6;
String result = "The sum of " + num1 + " and " + num2 + " is " + (num1 + num2) + ".";
System.out.println(result);
```

Tulostettu tulos on "The sum of 5 and 6 is 11.".

On myös tärkeää välittää merkkijonon ja muiden tietotyyppien väliset erot. Voit tehdä tämän käyttämällä String-metodia valueOf().

```Java
int num = 10;
String numString = String.valueOf(num);
System.out.println(numString);
```

Tulostettu tulos on "10".

## Katso myös

- [String Class - Oracle Help Center](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Concatenation in Java - GeeksforGeeks](https://www.geeksforgeeks.org/concatenation-in-java/)