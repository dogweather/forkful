---
title:    "C: Muuttaminen merkkijonoksi pienaakkosiksi"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi: Merkkijonon muuntaminen pieniksi kirjaimiksi

Merkkijonon muuntaminen pieniksi kirjaimiksi on tärkeää monissa ohjelmoinnin projekteissa. Se voi auttaa luomaan yhtenäisen ja selkeämmän koodin, sekä helpottaa merkkijonojen vertailua ja etsintää. 

## Miten: Esimerkkejä koodin muotoilusta

Merkkijonon muuntaminen pieniksi kirjaimiksi voidaan tehdä monella eri tavalla käyttäen C kielen kirjastoja. Alla on kolme esimerkkiä, jotka näyttävät eri tapoja muuntaa merkkijono pieniksi kirjaimiksi ja tulostaa se näytölle.

### Esimerkki 1:

```C
//Luodaan merkkijono
char string[] = "Merkkijono PIENILLÄ Kirjaimilla";

//muunnettaan merkkijono pieniksi kirjaimiksi käyttäen strlwr funktiota
strlwr(string);

//tulostetaan merkkijono näytölle
printf("Merkkijono: %s", string);

```

Tuloste: Merkkijono: merkkijono pienillä kirjaimilla

### Esimerkki 2:

```C
//Luodaan merkkijono
char string[] = "Tämä Merkkijono Muuntuu Martiksen kanssa";

//otetaan käyttöön ctype.h kirjasto
#include <ctype.h>

//Luodaan for-looppi, joka käy läpi merkkijonon
for (int i = 0; string[i]; i++) {
  //muutetaan jokainen kirjain pieneksi käyttäen tolower funktiota
  string[i] = tolower(string[i]);
}

//tulostetaan merkkijono näytölle
printf("Merkkijono: %s", string);

```

Tuloste: Merkkijono: tämä merkkijono muuntuu martiksen kanssa

### Esimerkki 3:

```C
//Luodaan merkkijono
char string[] = "MCMMXXVII";

//Luodaan for-looppi, joka käy läpi merkkijonon
for (int i = 0; string[i]; i++) {
  //muutetaan jokainen kirjain pieneksi käyttäen bitwise operaatiota
  string[i] |= 32;
}

//tulostetaan merkkijono näytölle
printf("Merkkijono: %s", string);

```

Tuloste: Merkkijono: mcmmxxvii

## Syvällisempi sukellus: Merkkijonon muuttaminen pieniksi kirjaimiksi

Merkkijonon muuntaminen pieniksi kirjaimiksi on tärkeää ymmärtää, sillä se vaikuttaa suoraan koodin ylläpidettävyyteen ja luettavuuteen. Käyttämällä sopivia kirjastoja ja funktioita, merkkijonoja voidaan muuntaa pieniksi kirjaimiksi ilman turhia komplikaatioita. Tärkeintä on myös ymmärtää, miten eri kielet käsittelevät merkkijonoja ja miten se voi vaikuttaa muuntamisprosessiin.

## Katso myös

- [C-kurssi (suomeksi)](https://www.tutorialspoint.com/cprogramming/)
- [Merkkijonon käsittely C-kielessä](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Merkkijonon käsittely kirjastoissa C-kielessä](https://www.thegeekstuff.com/2013/04/c-strings-interview-questions/)