---
title:                "C: Kirjoittaminen standarivirheelle"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Kirjoittamalla standardivirheeseen, voit ohjelmassasi näyttää virheilmoituksia ja varoitussanomia, jotka auttavat sinua vianjäljityksessä. Tämä on tärkeä osa ohjelmointia ja auttaa sinua löytämään ja korjaamaan mahdolliset virheet.

## Näin teet sen

Ohjelmassasi, voit käyttää funktion `fprintf` ja antaa toisena parametrinaan `stderr`-merkkijonon. Alla on yksinkertainen esimerkki, jossa kirjoitetaan virheilmoitus standardivirheeseen:

```C
#include <stdio.h>

int main() {
  fprintf(stderr, "Virhe: Tiedostoa ei löydy\n");
  return 0;
}
```

Tämän ohjelman ajaessa saat seuraavan tulosteen:

```
Virhe: Tiedostoa ei löydy
```

## Syvempi sukellus

Standardivirheeseen kirjoittaminen eroaa tavallisesta tulostamisesta siksi, että se ei ole sidottu tiettyyn kanavaan, kuten näyttöön tai tiedostoon. Tämä tekee siitä hyödyllisen virheiden käsittelyssä, koska voit ohjata virheilmoituksesi eri tarkoituksiin riippuen siitä, missä ohjelmasi suoritetaan.

Lisäksi on tärkeää hallita standardivirheen tulostusta, jotta se ei sekoitu tavallisen tulostuksen kanssa. Voit suorittaa virheen ohjauksen käyttäen `stderr`-kanavaa ja samalla tulostaa tavallinen teksti `stdout`-kanavalle.

## Katso myös

- [C-funktioiden dokumentointi](https://www.tutorialspoint.com/c_standard_library/index.htm)
- [Virheiden käsittely C-ohjelmoinnissa](https://www.geeksforgeeks.org/error-handling-c-programs/)
- [Standardivirheen käyttö C-ohjelmoinnissa](https://www.gnu.org/software/libc/manual/html_node/Standard-Error-Output.html)