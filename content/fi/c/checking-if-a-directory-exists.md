---
title:                "Tarkista löytyykö hakemistoa"
html_title:           "C: Tarkista löytyykö hakemistoa"
simple_title:         "Tarkista löytyykö hakemistoa"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Tiedoston olemassaolon tarkistaminen on prosessi, jossa ohjelmoija tarkistaa, onko annettu hakemisto olemassa. Tämä on tärkeää, jotta ohjelma voi käsitellä olemassa olevia tiedostoja tai luoda uusia, jos hakemistoa ei ole olemassa.

## Miten:
```C 
if (access(directory_path, F_OK) != -1) {
  printf("Hakemisto löytyi!");
} else {
  printf("Hakemistoa ei ole olemassa.");
}
```
Koodiesimerkissä käytetään "access" -funktiota, joka tarkistaa, onko hakemisto olemassa käyttämällä "F_OK"-parametria. Jos tiedosto löytyy, funktio palauttaa 0. Muussa tapauksessa se palauttaa -1 ja tulostaa viestin "Hakemistoa ei ole olemassa."

## Syvemmälle:
Tiedoston olemassaolon tarkistaminen on osa tiedostojärjestelmien peruskäsitteitä. Se antaa ohjelmoijalle mahdollisuuden hallita tiedostoja ja hakemistoja tehokkaasti.  On myös muita tapoja tarkistaa tiedoston olemassaolo, kuten käyttämällä "stat" -funktiota tai tutkimalla tiedostopolun olemassaoloa eri järjestelmissä.

## Katso myös:
Lisätietoja tiedostojärjestelmistä ja niiden hallinnasta löydät täältä:
- https://www.gnu.org/software/libc/manual/html_node/File-Attributes.html#File-Attributes
- https://linux.die.net/man/2/access
- https://www.tutorialspoint.com/c_standard_library/c_function_access.htm