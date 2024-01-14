---
title:                "Java: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnissa tarvitaan erilaisten tekstien käsittelyä. Yksi tärkeä osa tästä on tekstin muuntaminen pieniksi kirjaimiksi, minkä avulla voidaan helpottaa esimerkiksi syötteiden vertailua ja hakemista. Java-ohjelmoinnissa tähän tarkoitukseen löytyy valmiina funktio, joka muuntaa merkkijonon pieniksi kirjaimiksi.

## Miten

Muuntamalla merkkijono pieniksi kirjaimiksi käytetään Java-ohjelmoinnissa toLowercase funktiota. Seuraava esimerkki näyttää miten tämä tapahtuu:

```Java
String teksti = "HELLO WORLD";
String muunnettu = teksti.toLowerCase();
System.out.println(muunnettu);
```

Tulostus: hello world

Funktio muuntaa kaikki merkkijonon kirjaimet pieniksi kirjaimiksi ja palauttaa uuden merkkijonon. Tällä tavalla tekstin käsittely on helpompi toteuttaa ja eri syötteiden vertailu on tarkempaa.

## Syvempi sukellus

Tarkemmin funktiossa toLowercase käytetään Unicode-standardia, joka mahdollistaa erilaisten kirjoitusmuotojen huomioimisen. Unicode-standardi huomioi myös sellaiset kirjaimet, jotka eivät perinteisesti ole näkyvissä näppäimistöllä, kuten aksentit ja erikoismerkit. Tämä tekee funktiosta toLowercase luotettavan ja monipuolisen.

On myös hyvä huomata, että funktio ei muokkaa alkuperäistä merkkijonoa vaan palauttaa uuden muunnetun merkkijonon. Tämä tarkoittaa sitä, että mikäli alkuperäistä merkkijonoa tarvitaan vielä myöhemmin ohjelmassa, se säilyy muuttumattomana.

## Katso myös

- Java String Class: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/lang/String.html
- Unicode Standard: https://unicode.org/standard/standard.html