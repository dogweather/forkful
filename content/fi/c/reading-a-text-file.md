---
title:                "C: Tekstitiedoston lukeminen"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi
Tekstitiedoston lukeminen on tärkeä osa C-ohjelmoinnin oppimista ja ymmärtämistä. Se on yleinen tehtävä, joka auttaa ymmärtämään ohjelmien toimintaa ja miten tietoa voidaan lukea ja käsitellä.

## Miten 
Lukeminen tekstitiedostosta C-kielellä on yksinkertainen prosessi. Se alkaa avaamalla tiedosto käyttäen fopen-funktiota, joka ottaa kaksi argumenttia: tiedostonimen ja tilan. Tila on yleensä "r", joka tarkoittaa lukutilaa.

```
FILE *tiedosto;
tiedosto = fopen("tekstitiedosto.txt", "r");
```

Tässä esimerkissä olemme luoneet osoittimen tiedosto-nimiseen FILE-tyyppiseen rakenteeseen ja avanneet tiedoston lukutilassa. Seuraavaksi voimme käyttää fscanf-funktiota lukeaksemme tiedostosta tietoa muuttujiin.

```
int numero;
fscanf(tiedosto, "%d", &numero);
```
Tämä lukutapa lukee tiedostosta kokonaisluvun ja tallentaa sen muuttujaan numero.

## Syvempi sukellus
C-kielessä on useita tapoja lukea tekstitiedostoja, esimerkiksi fgets-funktio, joka lukee yhden rivin kerrallaan. Lisäksi on myös muita tapoja käsitellä luettua tietoa, kuten vertailemalla sitä muihin muuttujiin tai tallentamalla se taulukkoon.

Tärkeä muistaa on myös sulkea tiedosto, kun olet lopettanut sen lukemisen. Tämä tehdään fclose-funktiolla.

```
fclose(tiedosto);
```

Voit myös tarkistaa onnistuneen tiedoston avaamisen tarkistamalla, palauttaako fopen-funktio NULL-arvon. Jos näin on, tiedosto ei ole avattu oikein.

## Katso myös
- [C-kielen resurssit](https://fi.wikibooks.org/wiki/C)
- [Fopen-funktion dokumentaatio](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [Esimerkkejä tiedoston lukemisesta C:ssä](https://www.programiz.com/c-programming/c-file-input-output)