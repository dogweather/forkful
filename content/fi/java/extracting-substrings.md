---
title:                "Alaryhmien erottaminen"
html_title:           "Java: Alaryhmien erottaminen"
simple_title:         "Alaryhmien erottaminen"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Joskus tietojen käsittelyssä tarvitaan tietyn osan tekstistä tai merkkijonosta, esimerkiksi nimen tai puhelinnumeron erottelua. Tämä tapahtuu substrin purkamisen avulla ja tässä artikkelissa käymme läpi miten se tehdään Java-kielellä.

## Kuinka tehdä

```Java
// Luodaan merkkijono-olio ja annetaan sille sisältö
String teksti = "Tervetuloa Java-maailmaan!";

// Haetaan ensimmäiset 7 merkkiä merkkijonosta
String tekstinAlku = teksti.substring(0, 7);

// Tulostetaan teksti - "Tervetu"
System.out.println(tekstinAlku);

// Haetaan viimeiset 8 merkkiä merkkijonosta
String tekstinLoppu = teksti.substring(teksti.length() - 8);

// Tulostetaan teksti - "maailmaan!"
System.out.println(tekstinLoppu);

// Haetaan merkkijonon osa, joka sijaitsee välillä "Java" ja "!"
String tekstinKeskiosa = teksti.substring(teksti.indexOf("Java") + 4, teksti.indexOf("!"));

// Tulostetaan teksti - "-maailmaan-"
System.out.println("-" + tekstinKeskiosa + "-");
```

Tässä esimerkissä luomme ensin merkkijono-olion ja annamme sille sisällön. Sitten käytämme `substring()`-metodia, joka ottaa parametreina aloitus- ja lopetuskohdat ja palauttaa halutun osan merkkijonosta. Voimme myös käyttää `indexOf()`-metodia löytääksemme tietyn merkkijonon sijainnin ja käyttää sitä parametrina `substring()`-metodissa.

## Syvällinen tarkastelu

`substring()`-metodi toimii niin, että se luo uuden merkkijono-olion halutusta osasta alkuperäistä merkkijonoa. Tämä tarkoittaa sitä, että vaikka muokkaamme tai poistamme alkuperäisen merkkijonon, substrin sisältö pysyy ennallaan. On myös tärkeää huomata, että `substring()` käyttää nollaperusteista indeksointia, eli ensimmäinen merkki sijaitsee indeksissä 0.

Voit myös käyttää `substring()`-metodia kahden parametrin sijasta vain yhdellä parametrilla, jolloin se palauttaa osan merkkijonosta halutusta indeksistä eteenpäin. Esimerkiksi `teksti.substring(10)` palauttaisi osan merkkijonosta "maailmaan!" alkaen kohdasta 10 eteenpäin.

## Katso myös

- [Java String Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- [Substring Tutorial by Codecademy](https://www.codecademy.com/courses/learn-java/lessons/java-arraylist/exercises/java-substring)