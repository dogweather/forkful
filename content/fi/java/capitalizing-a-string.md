---
title:    "Java: Merkkijonon suurennus"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Miksi: Miksi kannattaa tehdä merkkijonon kirjoitus isoiksi aakkosiksi

Merkkijonon kirjoittaminen isoiksi aakkosiksi on yleinen ohjelmointitekniikka, jota käytetään useissa Java-sovelluksissa. Se antaa mahdollisuuden muokata tekstin ulkonäköä ja tehdä siitä helpommin luettavaa ja yhtenäisempää. Lisäksi käyttäjät voivat tuntea itsensä mukavammaksi käyttämällä isoja kirjaimia tekstin lukemiseen.

# Miten: Merkkijonon kirjoittaminen isoiksi aakkosiksi Java-ohjelmoinnissa

Java tarjoaa muutaman eri tapaa muuttaa merkkijono isoiksi aakkosiksi. Tässä esimerkissä käytämme String.toUpperCase () -metodia.

```Java
String s = "tämä on esimerkki";

// muuta merkkijono isot aakkoset
String capitalized = s.toUpperCase();
System.out.println(capitalized);
```

Tämä koodi antaa seuraavan tulosteen:

```
TÄMÄ ON ESIMERKKI
```

Voit myös käyttää Character.toUpperCase () -metodia, jolla voit muuttaa yksittäinen kirjain isoksi.

```Java
char c = 'a';

// muuta kirjain isot aakkoset
char capitalized = Character.toUpperCase(c);
System.out.println(capitalized);
```

Tämä koodi antaa seuraavan tulosteen:

```
A
```

# Syvällinen erittely: Merkkijonon kirjoittaminen isoiksi aakkosiksi

Merkkijonon kirjoittamisen isoksi aakkosiksi Java-ohjelmoinnissa taustalla on Unicode-merkkijonojen standardi. Jokaisella tallennetulla merkillä on oma numeronsa Unicode-taulukossa ja pienet kirjaimet ovat peräkkäin isojen kirjainten kanssa.

Joten, kun käytät toUpperCase () -metodia, se tarkistaa jokaisen merkin Unicode-numeron ja jos se on pieni kirjain, muuttaa sen vastaavaksi isoksi kirjaimeksi.

On myös tärkeää huomata, että merkkijonot ovat muuttumattomia Java-ohjelmoinnissa, mikä tarkoittaa, että kun muutat merkkijonoa isoksi, sinun täytyy tallentaa muutettu merkkijono uuteen muuttujaan.

# Katso myös

- [Java String-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Character-luokka](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html)
- [Unicode standardi](https://unicode.org/standard/standard.html)