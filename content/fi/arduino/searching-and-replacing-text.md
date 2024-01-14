---
title:    "Arduino: Tekstin etsiminen ja korvaaminen"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi

Miksi etsiä ja vaihtaa tekstiä Arduino-ohjelmoinnissa? Yksinkertaisesti sanottuna, se on tärkeä taito, jota tarvitaan monissa ohjelmoinnin projekteissa. Se auttaa sinua muokkaamaan tekstiä nopeasti ja helposti ilman, että sinun täytyy kirjoittaa samaa koodia uudelleen ja uudelleen. Se myös auttaa sinua välttämään inhimillisiä virheitä, kun sinun täytyy muuttaa tiettyjä tekstejä ohjelmassa.

## Miten tehdä se

Etsi ja korvaa -toimintoa kutsutaan myös "String.replace()" Arduino-kielessä. Se toimii yksinkertaisesti korvaamalla annetun merkkijonon toisella merkkijonolla. Tässä on esimerkki siitä, miten tehdä se ```Arduino String``` esimerkin avulla:

```
Arduino String muuttuja = "Tervetuloa maailma!";
muuttuja.replace("Tervetuloa", "Hei");
Serial.println(muuttuja);
```

Tuloste olisi "Hei maailma!". Huomaat, että "replace" -toiminto korvaa ensimmäisen merkkijonon toisella annetulla merkkijonolla. Voit myös käyttää "String.indexOf()" funktiota löytääksesi tietyn merkkijonon indeksin ja korvata sen. Katso esimerkki:

```
Arduino String aika = "Tänään on maanantai";
int indeksi = aika.indexOf("maanantai");
if(indeksi >= 0) {
  aika.replace(indeksi, indeksi+12, "tiistai");
}
Serial.println(aika);
```

Tuloste olisi "Tänään on tiistai". Huomaat, että "replace" -toiminto korvaa annetun indeksin ja tietyn pituuden merkkijonolla, joka annetaan sen jälkeen.

## Syvemmälle

Itse asiassa "String.replace()" -toimintoa käytetään taustalla "String.concat()" -toiminnon avulla. Tämä tarkoittaa, että voit myös ketjuttaa useita "replace" -toimintoja yhteen etuliitteen ja jälkiliitteen lisäämiseksi merkkijonoon. Katso esimerkki:

```
Arduino String teksti = "Tervetuloa käyttäjänimi!";
text.replace("käyttäjänimi", "Bob").replace("Hei", "Hei, ").replace("!", ".");
Serial.println(teksti);
```

Tuloste olisi "Hei, Bob.".

## Katso myös

- [String.replace() reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/replace/)
- [String.concat() reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/concat/)
- [String.indexOf() reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)
- [Official Arduino website](https://www.arduino.cc/)