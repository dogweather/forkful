---
title:    "C: Aiheena on nimien erottelu"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi erottaa osamerkkijonoja? Yleisesti ottaen tämä on hyödyllistä, kun käsitellään suuria merkkijonoja ja halutaan erottaa ja työstää niitä pienemmissä paloissa. Tämä tekniikka on erityisen tärkeä tekstinkäsittelyssä, kuten tietokoneohjelmien kirjoittamisessa ja data-analyysissä.

## Näin teet sen

 Substringien erottaminen C-kielellä on melko yksinkertaista käyttäen muutamia perusfunktioita. Tässä on muutama esimerkki: 
```C 
// Alustaa merkkijonon
char string[] = "Tämä on esimerkki merkkijonosta."; 
// Määrittele uusi merkkijono substringiksi
char substring[5]; 
// Käytä strncpy-funktiota kopioidaksesi vain osa merkkijonosta
strncpy(substring, string + 8, 5); 
// Tulosta substring
printf("Substring: %s\n", substring); 
// Output: on es
```

Tässä ensin alustetaan merkkijono ja määritellään uusi merkkijono, johon haluamme kopioida osamerkkijonon. Sitten käytämme strncpy-funktiota, joka ottaa kolme parametria: kohdesubstring, lähteensubstring ja kuinka monta merkkiä haluat kopioida. Lopuksi tulostetaan tulos. Huomaa, että merkkijonon indeksointi alkaa aina nollasta, joten kun haluamme kopioida esimerkissä "on es", käytämme lähteensubstringin parametrina indeksejä 8 ja 5. 

## Syvä sukellus

Voit myös käyttää muita C:n muutettuja merkkijonofunktioita, kuten strncat ja strtok, muodostamaan ja erottamaan useampia osiota merkkijonosta. Voit myös käyttää muuttujia, kuten strlen, jotta voit tehdä osamerkkijonon koon dynaamiseksi. On myös tärkeää ottaa huomioon merkkijonojen lopettavat merkit, kuten nolla-merkki, kun työskentelet niiden kanssa substringeina.

## Katso myös

* [C-kielen merkkijonofunktiot](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
* [Merkkijonojen hallinta C-kielessä](https://www.studytonight.com/c/string-management-functions.php)
* [C-kieen muokatut merkkijonofunktiot](https://www.geeksforgeeks.org/c-string-manipulation-c-strlcpy/)