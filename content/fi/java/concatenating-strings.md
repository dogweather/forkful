---
title:    "Java: Merkkijonojen yhdistäminen"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit yhdistää merkkijonoja? Yhdistäminen on hyödyllinen työkalu, jota käytetään usein Java-ohjelmoinnissa. Se mahdollistaa erilaisten tietojen yhdistämisen yhdeksi merkkijonoksi, mikä voi olla erittäin hyödyllistä tulostuksen yhteydessä.

## Miten

Yhdistäminen voidaan tehdä helposti Java-kielen sisäänrakennetun concat() -metodin avulla. Se ottaa parametreiksi haluttujen merkkijonojen määrän ja yhdistää ne yhdeksi merkkijonoksi. Alla on esimerkki koodista ja sen tulostuksesta:

```Java
// Alustetaan kaksi merkkijonoa
String hello = "Hei";
String world = "maailma!";

// Yhdistetään merkkijonot concat() metodilla
String helloWorld = hello.concat(" ").concat(world);

// Tulostetaan yhdistetty merkkijono
System.out.println(helloWorld);
```

Tulostus:
```
Hei maailma!
```

## Syvempi sukellus

Java mahdollistaa myös merkkijonojen suoran yhdistämisen "+" -operaattorin avulla. Tämä tekee koodista hieman siistimpää ja helpommin luettavaa. Alla olevassa esimerkissä käytetään myös yhdistettyä sijoitusoperaattoria "+=".

```Java
// Alustetaan kaksi merkkijonoa
String language = "Java";
String version = "8";

// Suora yhdistäminen + -operaattorilla
String javaVersion = "This is " + language + " version " + version;

// Yhdistetty sijoitusoperaattori +=
javaVersion += "!";
// Tämä vastaa javaVersion = javaVersion + "!"

// Tulostetaan yhdistetty merkkijono
System.out.println(javaVersion);
```

Tulostus:
```
This is Java version 8!
```

On myös tärkeää huomata, että concat() -metodi luo uuden merkkijonon jokaisen yhdistämisen jälkeen, kun taas "+" -operaattori muokkaa alkuperäistä merkkijonoa.

## Katso myös

- Java String concat() metodi: https://www.javatpoint.com/java-string-concat
- String concatenation in Java: https://www.geeksforgeeks.org/string-concatenation-java/