---
title:    "Java: Poistetaan kaavan mukaiset merkkijonot"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi joku haluaisi poistaa merkkejä, jotka vastaavat tiettyä kaavaa ohjelmointitehtävässä. Ehkä haluat puhdistaa merkkijonon ennen sen käyttämistä tai tunnistaa tietynlaisia merkkejä tietokannassa. Tässä blogipostissa keskitymme opettamaan, kuinka voit poistaa merkkejä tietyn kaavan perusteella käyttämällä Java-ohjelmointikieltä.

## Kuinka tehdä

Poistaminen karaktereita käyttäen kaavaa Java-ohjelmointikielellä voidaan tehdä monella tavalla, mutta tässä esittelemme yksinkertaisimman tavan käyttää `String.replaceAll()`-metodia. Tämä metodi korvaa tekstissä esiintyvät merkit, jotka vastaavat annettua kaavaa, uudella merkkijonolla.

Esimerkiksi, kun haluat poistaa kaikki numerot merkkijonosta, voit käyttää seuraavaa koodia:

```Java
String s = "Tämä on 1 esimerkki 2 poistettavista numeroista!";
s = s.replaceAll("[0-9]", "");
System.out.println(s);
```

Tulostus olisi:

```Java
Tämä on esimerkki poistettavista numeroista!
```

## Syvällinen tutkimus

`replaceAll()`-metodi perustuu säännöllisiin lausekkeisiin, jotka määrittelevät tietynlaisen kaavan merkkijonon poistamiseen. Tässä muutamia esimerkkejä säännöllisistä lausekkeista, joita voit käyttää `replaceAll()`-metodissa:

- `[A-Z]` poistaa kaikki isot kirjaimet merkkijonosta
- `[a-z]` poistaa kaikki pienet kirjaimet merkkijonosta
- `[0-9]` poistaa kaikki numerot merkkijonosta
- `[^\w\s]` poistaa kaikki erikoismerkit merkkijonosta
- `is` poistaa kaikki esiintymät tekstin "is"

Kun käytät säännöllisiä lausekkeita, voit myös käyttää `String[] split()`-metodia, joka pilkkoo merkkijonon kaavaa vastaavien merkkien kohdalta ja palauttaa taulukon merkkejä.

## Katso myös

- [Säännölliset lausekkeet Java-ohjelmoinnissa](https://www.tutorialspoint.com/java/java_regular_expressions.htm)
- [Java String-luokan dokumentaatio](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [String.replaceAll() Java-ohjelmoinnissa](https://www.geeksforgeeks.org/string-replaceall-java-examples/)