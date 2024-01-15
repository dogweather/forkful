---
title:                "HTML:n jäsentäminen"
html_title:           "C: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Jos haluat kehittää ohjelmia, jotka käsittelevät verkkosivuja ja -sisältöä, on tärkeää ymmärtää HTML-koodia ja sen rakennetta. Tämä mahdollistaa sisällön parsimisen ja käsittelyn eri ohjelmointikielet, kuten C, avulla.

## Miten tehdä

HTML-koodin parsiminen C-kielellä voi tuntua haastavalta, mutta seuraavilla esimerkeillä ja selityksillä pääset alkuun!

```C
// Esimerkki HTML-dokumentista
<html>
  <head>
    <title>Tervetuloa</title>
  </head>
  <body>
    <h1>Hei maailma!</h1>
    <p>Tämä on esimerkkiteksti.</p>
  </body>
</html> 
```

Käyttämällä C:n kirjastoa <stdlib.h>, voimme avata ja lukea yllä olevan HTML-dokumentin. Tämän jälkeen voimme käyttää esimerkiksi <string.h> -kirjastoa etsiäksemme ja tulostaa haluamamme tiedot.

```C
#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 
  
int main() { 
    // Avataan tiedosto
    FILE *fptr = fopen("index.html", "r"); 
  
    // Luetaan tiedoston sisältö
    char c = fgetc(fptr); 
  
    // Merkkijonon alustus 
    char str[100]; 
    
    // Merkkijonon etsiminen ja tulostaminen
    while (c != EOF) {
        if (strstr(str, "<h1>") != NULL) {
            printf("Otsikko: %s\n", str); 
        }
        c = fgetc(fptr); 
    } 
  
    // Suljetaan tiedosto
    fclose(fptr); 
    return 0; 
}
```

**Esimerkkilähtö:**
```
Otsikko: Hei maailma!
```

## Syvemmälle HTML-parsintaan

HTML-koodissa on erilaisia tageja ja rakenteita, jotka voivat vaikuttaa parsinnan haastavuuteen. Esimerkiksi sisennykset ja ylimääräiset välilyönnit voivat aiheuttaa ongelmia merkkijonon etsimisessä. Tästä syystä on tärkeää olla huolellinen ja tarkkaavainen koodin kirjoittamisessa.

Lisäksi on syytä ottaa huomioon, että HTML-koodi on dynaamista ja sisältö voi vaihdella eri verkkosivuilla. Tämän vuoksi voi olla tarpeen tehdä useampia tarkistuksia ja säätää koodia vastaavasti.

## Katso myös

- [String.h dokumentaatio](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Stdlib.h dokumentaatio](https://www.tutorialspoint.com/c_standard_library/stdlib_h.htm)
- [HTML-opas aloittelijoille](https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web/HTML_basics)