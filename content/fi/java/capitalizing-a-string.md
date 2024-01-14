---
title:                "Java: Kirjoita merkkijono isolla alkukirjaimella"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi?

Joissain tapauksissa on tarpeen muuttaa merkkijonon kirjaimia muuttamalla ne isoiksi tai pieniksi. Tämä voi olla esimerkiksi tarpeellista, jos halutaan syöttää käyttäjän syöttämä teksti tietokantaan ja varmistaa, että se tallennetaan yhtenäisellä tavalla. Tai toisessa tapauksessa halutaan vain esittää tietoa käyttäjälle visuaalisesti isoina kirjaimina.

## Kuinka tehdä se?

Onneksi Javan String-luokka tarjoaa valmiin toiminnon merkkijonon muuttamiseen isoiksi kirjaimiksi. Tämä toiminto on nimeltään `toUpperCase()`. Se voidaan kutsua suoraan merkkijonoon ja se palauttaa uuden merkkijonon, jossa kaikki kirjaimet ovat isoja.

```Java
String teksti = "Hei Maailma!";
String isoilla = teksti.toUpperCase();

System.out.println(isoilla); // Tulostaa "HEI MAAILMA!"
```

Huomaa, että `toUpperCase()` ei muuta alkuperäistä merkkijonoa vaan palauttaa uuden muokatun version. Jos haluat muuttaa alkuperäisen merkkijonon isoiksi kirjaimiksi, voit korvata sen uudella versiolla:

```Java
String teksti = "Hei Maailma!";
teksti = teksti.toUpperCase();

System.out.println(teksti); // Tulostaa "HEI MAAILMA!"
```

## Syvemmälle pintaan

Vaikka `toUpperCase()`-toiminto on helppo käyttää, on hyvä ymmärtää hieman enemmän siitä, miten se toimii taustalla. Javan String-luokka tallentaa merkkijonot taulukkona, jossa jokainen merkki on erillinen elementti. `toUpperCase()`-toiminto käy läpi tämän taulukon ja muuntaa jokaisen merkin isoiksi vastaavien ASCII-koodien avulla.

On myös hyvä huomata, että `toUpperCase()` ei muuta muita merkkejä kuin kirjaimia. Numerot ja erikoismerkit säilyvät samoina. Tämä kannattaa ottaa huomioon, jos merkkijonossa on esimerkiksi välimerkkejä tai numeroita.

## Katso myös

- Javan String-luokan tiedot: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html
- ASCII-koodit: https://www.ascii-code.com/