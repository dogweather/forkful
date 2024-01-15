---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Java: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

JavaScript on suosittu ohjelmointikieli, jota käytetään laajasti web-kehittämisessä. Yksi yleinen tehtävä, johon JavaScriptillä törmää, on tekstin hakeminen ja korvaaminen. Tämä on hyödyllistä, kun haluat tehdä massamuutoksia koodissasi tai muokata käyttäjän syöttämää tekstiä ennen sen tallentamista tietokantaan. Tässä artikkelissa esittelemme, kuinka voit käyttää Javaa hakemaan ja korvaamaan tekstiä tehokkaasti.

## Kuinka

Hakeminen ja korvaaminen voidaan suorittaa käyttämällä String-luokan replaceAll-metodia. Se toimii seuraavasti:

```java
String originalString = "Tervetuloa maailmaan!";
String newString = originalString.replaceAll("maailmaan", "uuteen maailmaan");

System.out.println(newString);
```

Tässä esimerkissä korvaamme alkuperäisen merkkijonon "maailmaan" uudella merkkijonolla "uuteen maailmaan". Output on "Tervetuloa uuteen maailmaan!"

Voit myös käyttää replaceAll-metodia säännöllisillä lausekkeilla. Tämä on hyödyllistä muokattaessa monimutkaisempia merkkijonoja. Alla on esimerkki käyttämällä säännöllistä lauseketta poistaaksesi kaikki erikoismerkit merkkijonosta:

```java
String originalString = "Tämä on erittäin##spesiaali#merkkijono!";
String newString = originalString.replaceAll("\\\\W", "");

System.out.println(newString);
```

Tässä käytämme säännöllistä lauseketta "\\W", joka poistaa kaikki erikoismerkit. Output on "Tämäonerittäinspesiaalimerkkijono!".

## Syväsukellus

String-luokan replaceAll-metodi hyödyntää säännöllisiä lausekkeita suorittaessaan hakua ja korvaamista. Säännölliset lausekkeet ovat kätevä työkalu monimutkaisiin haku- ja korvaustehtäviin, ja niitä kannattaa opiskella tarkemmin.

On myös tärkeää huomata, että replaceAll-metodi palauttaa uuden merkkijonon, eikä muuta alkuperäistä merkkijonoa. Jos haluat muuttaa alkuperäistä merkkijonoa, voit käyttää String-luokan replace-metodia.

## Katso myös

- [Java String-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Säännölliset lausekkeet Java:ssa](https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html)