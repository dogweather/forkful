---
title:                "Java: Alimerkkijonojen erottaminen"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi
Substringien erottaminen on tärkeä osa Java-ohjelmointia ja tarjoaa monia hyödyllisiä toimintoja, kuten merkkijonojen käsittelyä ja manipulointia sekä tiedon hakua tietyistä osista merkkijonoja. Tällä blogipostauksella haluamme antaa sinulle hyödyllisiä vinkkejä ja esimerkkejä siitä, miten voit käyttää substringien erotusominaisuuksia Java-ohjelmissasi.

## Miten
Substringien erottaminen Java-ohjelmassa on helppoa. Sinun tarvitsee vain käyttää String-luokan substring-metodia ja määrittää haluamasi aloituskohdat ja lopetuskohdat. Seuraavassa esimerkissä näet, miten voit erottaa osamerkkijonon alkuperäisestä merkkijonosta:

```Java
String originalString = "Tervetuloa Java-maailmaan!";
String substring = originalString.substring(13);
System.out.println(substring); // Tulostaa "Java-maailmaan!"
```

Voit myös määrittää aloitus- ja lopetusindeksit, jotta voit valita haluamasi osan merkkijonosta. Seuraavassa esimerkissä erottamme sanan "Java" alkuperäisestä merkkijonosta:

```Java
String originalString = "Tervetuloa Java-maailmaan!";
String substring = originalString.substring(13, 17);
System.out.println(substring); // Tulostaa "Java"
```

## Syväsukellus
Halutessasi voit myös käyttää String-luokan erilaisia substring-metodeja, kuten subSequence(), indexOf() ja lastIndexOf(). Nämä metodit tarjoavat erilaisia tapoja erottaa merkkijonoja ja voit valita niistä parhaiten tarpeisiisi sopivan.

On myös tärkeää huomata, että substringit ovat muuttumattomia, eli ne eivät muuta alkuperäistä merkkijonoa, vaan palauttavat uuden merkkijonon.

## Katso myös
- [Java String-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Substringien käyttö String-luokassa](https://www.w3schools.com/java/java_strings_substrings.asp)
- [Substringit ja StringBuilder-luokka](https://www.baeldung.com/java-substring)