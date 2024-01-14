---
title:    "Java: Kirjoittaminen standardivirheeseen."
keywords: ["Java"]
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi kirjoittaa stanardivirheeseen (standard error) Java-ohjelmoinnissa. Yksi yleisimmistä syistä on virheiden löytäminen ja korjaaminen ohjelmassa. Standardivirheen käyttö voi auttaa kehittäjää selvittämään, missä kohdassa koodia virhe tapahtuu ja mahdollisesti miksi se tapahtuu.

## Kuinka

Käyttäen Javaa, voit kirjoittaa stanardivirheeseen käyttämällä System.err.println() -komennolla. Tämä tulostaa annetun viestin standardivirheeseen. Voit myös käyttää System.err.print() -komennolla jos haluat tulostaa viestin ilman rivinvaihtoa.

```Java
System.err.println("Tämä on virheviesti");
System.err.print("Tämä on virheellinen rivi");
```
**Tuloste:**
```
Tämä on virheviesti
Tämä on virheellinen rivi
```

## Syvempi sukellus

Java-kehittäjän tulee olla tietoinen, että stanardivirheeseen tulostetut viestit eivät näy samassa paikassa kuin normaalit System.out.println() -komennolla tulostetut viestit. Standardivirheeseen tulostetut viestit näkyvät yleensä punaisella tekstillä konsolissa ja ne voivat sisältää lisätietoa virheestä, kuten virheilmoituksen ja virhekoodin.

Standardivirheeseen tulostettavien viestien hyödyntäminen auttaa myös kehittäjää tunnistamaan ja seuraamaan ongelmia koodissa, erityisesti suurissa ohjelmissa, joissa virheiden jäljittäminen voi olla haastavaa.

## Katso myös

- [Java System Class](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Java PrintStream Class](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)
- [Understanding Java System.out.println() and System.err.println()](https://javabeginnerstutorial.com/core-java-tutorial/system-class-in-java/)

Kiitos lukemisesta! Toivottavasti tämä artikkeli auttaa sinua kehittäjänä kirjoittaessasi stanardivirheeseen Java-ohjelmissasi. Muista käyttää tätä työkalua vain tarpeen mukaan ja pyrkiä korjaamaan ja ehkäisemään virheitä jo koodin suunnitteluvaiheessa. Onnea ohjelmointiin!