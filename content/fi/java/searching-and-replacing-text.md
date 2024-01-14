---
title:    "Java: Tekstin etsiminen ja korvaaminen"
keywords: ["Java"]
---

{{< edit_this_page >}}

# Miksi

Miksi etsitään ja vaihdetaan tekstiä? Joskus meillä on tarve muokata suuria tekstimassoja nopeasti ja tehokkaasti. Tällöin tekstien etsiminen ja korvaaminen on erittäin kätevä tapa saada halutut muutokset tehtyä nopeasti.

# Miten

Tässä osiossa käydään läpi konkreettisia esimerkkejä siitä, miten etsiä ja vaihtaa tekstiä Javalla. Käytämme esimerkimme `replace()`-metodia, joka on osa `String`-luokan toiminnallisuutta.

````Java
public static void main (String[] args) { 
  // Luodaan esimerkkiteksti
  String teksti = "Hei kaikille Java-faneille! Tervetuloa oppimaan uutta!";
  
  // Etsitään ja vaihdetaan teksti
  String uusiTeksti = teksti.replace("Java", "Python");
  
  // Tulostetaan uusi teksti
  System.out.println(uusiTeksti);
}
````

Tämä koodipätkä tulostaisi seuraavan tekstin:

```
Hei kaikille Python-faneille! Tervetuloa oppimaan uutta!
```

Näin yksinkertaisesti tekstien etsiminen ja korvaaminen onnistuu Javalla.

# Syvällinen sukellus

Ehkä haluat tehdä enemmän kuin vain yksinkertaisen korvaamisen. Javalla on mahdollista käyttää regular expressioneita (säännöllisiä lausekkeita) tekstien etsimiseen ja korvaamiseen. Tämä antaa sinulle paljon laajemman ja tarkemman hallinnan siitä, mitä etsit ja mitä korvaat.

Regular expressionit ovat sääntöjä, jotka määrittelevät, millaisia merkkijonoja haluat löytää. Tässä esimerkissä etsimme kaikki sanat, jotka alkavat isolla kirjaimella ja loppuvat huutomerkkiin.

````Java
public static void main (String[] args) {
  // Luodaan esimerkkiteksti
  String teksti = "Hei kaikille Java-faneille! Tervetuloa oppimaan uutta!";
  
  // Etsitään ja vaihdetaan teksti regular expressionin avulla
  String uusiTeksti = teksti.replaceAll("\\b[A-Z][a-z]*[!]\\b", "Python");
  
  // Tulostetaan uusi teksti
  System.out.println(uusiTeksti);
}
````

Tämä koodipätkä tulostaisi seuraavan tekstin:

```
Hei kaikille Python-faneille! Tervetuloa oppimaan uutta!
```

Syvällisessä sukelluksessa on mahdollista käyttää monimutkaisempia regular expressioneita ja luoda monipuolisia tekstien etsimisen ja korvaamisen toimintoja.

# Katso myös

- [Oracle Java - String luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Regular Expressions](https://www.tutorialspoint.com/java/java_regular_expressions.htm)
- [Regular-Expressions.info](https://www.regular-expressions.info)