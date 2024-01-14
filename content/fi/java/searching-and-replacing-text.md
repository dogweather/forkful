---
title:    "Java: Tekstin etsiminen ja korvaaminen"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi
Tekstin etsiminen ja korvaaminen ovat olennainen osa Java-ohjelmointia. Ne mahdollistavat tietyn merkkijonon tai sanan korvaamisen toisella halutulla merkkijonolla, mikä auttaa esimerkiksi korjaamaan oikeinkirjoitusvirheitä tai muuttamaan vanhoja tekstejä uudempaan muotoon. Tämä blogipostaus opastaa, miten voit tehdä tämän kätevästi Java-koodillasi.

## Näin
Java-ohjelmoijalla on käytössään muutamia eri metodeja tekstin etsimiseen ja korvaamiseen. Yksi näistä metodeista on `replaceAll()`-metodi, jonka avulla voit korvata kaikki halutut merkkijonot toisilla merkkijonoilla haluamallasi tavalla.

Esimerkiksi, jos haluat korvata kaikki "kissa"-sanan esiintymät tekstin sisällä "koira"-sanalla, voit käyttää seuraavaa koodia:

```Java
String teksti = "Tänään näin pienen kissan puistossa.";
String uusiTeksti = teksti.replaceAll("kissa", "koira");
System.out.println(uusiTeksti);
```

Tämä koodi tulostaa "Tänään näin pienen koiran puistossa."

Toinen tapa etsiä ja korvata tekstejä on käyttää `replace()`-metodia, joka ottaa vastaan kaksi parametria: vanhan tekstin ja uuden tekstin. Tämän metodin avulla voit korvata vain yhden tietyn merkkijonon kerrallaan.

```Java
String teksti = "Minulla on musta koira.";
String uusiTeksti = teksti.replace("musta", "valkoinen");
System.out.println(uusiTeksti);
```

Tämä koodi tulostaa "Minulla on valkoinen koira."

Voit myös käyttää `startsWith()`- ja `endsWith()`-metodeja tarkistaaksesi, alkaako tai päättyykö tekstisi tietyllä merkkijonolla. Nämä metodit voivat olla hyödyllisiä esimerkiksi silloin, kun haluat korvata tietyn sanan vain tietyssä kohdassa tekstiä.

## Syvällinen sukellus
Java tarjoaa monia vaihtoehtoja tekstin etsimiseen ja korvaamiseen, ja erilaiset metodit sopivat erilaisiin tilanteisiin. On tärkeää pohtia, mikä metodi sopii parhaiten juuri omaan tarkoitukseesi. Lisäksi voit käyttää myös säännöllisiä lausekkeita tarkemman tekstin etsimisen ja korvaamisen kanssa.

Muistathan myös aina testata koodisi erilaisilla syötteillä ja tarkistaa, että haluttu tulos saavutetaan.

## Katso myös
- [Oracle Java-tietojenkäsittelyopas](https://docs.oracle.com/javase/tutorial/essential/regex/index.html)
- [Java String-luokan dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Säännölliset lausekkeet Java:ssa](https://www.vogella.com/tutorials/JavaRegularExpressions/article.html)