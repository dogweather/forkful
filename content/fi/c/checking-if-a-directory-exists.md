---
title:                "Kansion olemassaolon tarkistaminen"
html_title:           "C: Kansion olemassaolon tarkistaminen"
simple_title:         "Kansion olemassaolon tarkistaminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Miksi

On monia syitä miksi voit haluta tarkistaa, onko hakemisto olemassa C-ohjelmointikielessä. Yksi tärkeimmistä syistä on varmistaa, että ohjelma toimii oikein, jos se kohdistaa tiedostohakemenon johonkin tiettyyn polkuun.

# Miten tehdä

Tässä on yksinkertainen esimerkki, kuinka voit tarkistaa, onko hakemisto olemassa C: ssä:

```
#include <stdio.h>
#include <stdbool.h> // tarvitaan bool-tietotyyppi

int main()
{
  char* polku = "/home/käyttäjä/testi";
  bool onko_hakemisto = false;

  // avataan hakemisto polun avulla
  FILE* hakemisto = fopen(polku, "r"); 

  if(hakemisto == NULL) // jos hakemisto ei löydy
  {
    onko_hakemisto = false;
  }
  else 
  {
    onko_hakemisto = true;
    fclose(hakemisto); // suljetaan hakemisto
  }

  if(onko_hakemisto) // tulostetaan vastaava viesti 
  {
    printf("Hakemisto %s on olemassa.\n", polku);
  }
  else
  {
    printf("Hakemistoa %s ei löydy.\n", polku);
  }

  return 0;
}
```

Esimerkissä avataan hakemisto "testi" ja tarkistetaan, onko se olemassa. Tämän jälkeen tulostetaan vastaava viesti riippuen siitä, löytyikö hakemisto vai ei.

Esimerkkitulostus:
```
Hakemisto /home/käyttäjä/testi on olemassa.
```

# Syvemmälle sukeltaminen

Tarkistaaksesi, onko hakemisto olemassa C: ssä, voit käyttää "fopen" -funktiota ja tarkistaa, onko se palauttanut oikean arvon. Jos funktion paluuarvo on NULL, tämä tarkoittaa, että hakemisto ei ole olemassa. Muussa tapauksessa se on olemassa.

On myös huomattava, että on olemassa muita tapoja tarkistaa hakemiston olemassaolo, kuten käyttämällä "opendir" -funktiota ja "stat" -funktiota. Jokaisella näistä tavoista on omat etunsa ja haittansa ja siksi on hyvä tutustua useampaan tapaan ennen kuin valitset tietyn menetelmän.

# Katso myös

- [fopen C-kirjastossa](https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm)
- [C-tiedostonhallinnan perusteet](https://www.geeksforgeeks.org/basics-file-handling-c/)
- [Esimerkkejä hakemistojen tarkistamisesta C: ssä](https://www.includehelp.com/c/file-handling-programs-to-check-file-status-fopen-fclose.aspx)