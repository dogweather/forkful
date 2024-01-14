---
title:    "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi."
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Miksi

Ehkä olet törmännyt tilanteeseen, jossa haluat muuttaa jonkin syötteeksi saamasi merkkijonon pieniksi kirjaimiksi. Tämä voi olla hyödyllistä esimerkiksi vertaillessasi syötteitä tai tulostettaessa tietoa näytölle. Tässä blogikirjoituksessa opimme, miten tehdä tämä Arduino-ohjelmoinnissa.

## Miten

Merkkijonon muuttaminen pieniksi kirjaimiksi on mahdollista Arduino-ohjelmassamme käyttämällä C++-kielen funktioita. Esimerkiksi voimme käyttää `toLowerCase()`-funktiota, joka muuttaa kaikki merkkijonon kirjaimet pieniksi kirjaimiksi.

```Arduino
String s = "HELLO WORLD";
s = s.toLowerCase();
Serial.println(s);

// Output:
// hello world
```

Tässä esimerkissä olemme ensin luoneet merkkijonomuuttujan `s`, joka sisältää tekstin "HELLO WORLD". Sitten käytämme `toLowerCase()`-funktiota muuttamaan muuttujan `s` sisältämät kirjaimet pieniksi kirjaimiksi. Lopuksi tulostamme muuttujan `s` sisällön sarjaporttiin.

On myös huomattava, että `toLowerCase()`-funktio muuttaa vain kirjaimet, jotka ovat kirjaimiksi tunnistettavia ASCII-koodinumeroita. Esimerkiksi merkkiä "!" ei muuteta, koska se ei ole kirjain.

## Syvempi sukellus

Olemme juuri oppineet, miten käyttää `toLowerCase()`-funktiota pieniksi kirjaimiksi muuttamiseen, mutta miten tämä toimii taustalla? C++:ssa jokaisella merkillä on vastaava ASCII-koodinumero, ja `toLowerCase()`-funktio tarkistaa, onko kunkin merkin koodinumero välillä 65-90 (suuret kirjaimet). Jos se on, se vähentää 32 koodinumerosta ja muuttaa kirjaimen pieneksi kirjaimeksi.

Voit myös kirjoittaa oman funktion, joka muuttaa merkkijonon pieniksi kirjaimiksi iteroiden jokaisen merkin yli ja suorittamalla muutoksen manuaalisesti. Tämä voi olla hyödyllistä opetella, mutta `toLowerCase()`-funktion käyttö on yksinkertaisempi ja tehokkaampi vaihtoehto.

Nyt olet valmis aloittamaan merkkijonojen muuntamisen pieniksi kirjaimiksi Arduino-ohjelmoinnissa!

## Katso myös

- [ASCII-taulukko](https://fi.wikipedia.org/wiki/ASCII)
- [C++ kirjastot Arduino-ohjelmoinnissa](https://www.arduino.cc/reference/en/#libraries)