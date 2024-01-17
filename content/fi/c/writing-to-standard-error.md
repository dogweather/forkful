---
title:                "Standard errorin kirjoittaminen"
html_title:           "C: Standard errorin kirjoittaminen"
simple_title:         "Standard errorin kirjoittaminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Kirjoittaminen standardivirheeseen on tapa ilmoittaa ohjelmassa tapahtuneesta virheestä tai epäonnistuneesta toiminnasta. Tätä käytetään usein ohjelmoinnissa, jotta voidaan helposti tunnistaa ja korjata virheitä.

## Ohjeet:

### Esimerkki 1:
```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Tämä on esimerkki kirjoittamisesta standardivirheeseen.");
    return 0;
}

```
### Tulostus:
```
Tämä on esimerkki kirjoittamisesta standardivirheeseen.
```

### Esimerkki 2:
```C
#include <stdio.h>

void function() {
    fprintf(stderr, "Toinen esimerkki.");
}

int main() {
    function();
    return 0;
}

```
### Tulostus:
```
Toinen esimerkki.
```

## Syväsukellus:

Kirjoittaminen standardivirheeseen on ollut osa C-ohjelmointia jo pitkään. Sitä käytetään usein yhdessä ```fprintf```-funktion kanssa, joka mahdollistaa tarkemman virheen ilmoittamisen. On myös olemassa muita tapoja käsitellä virheitä, kuten käyttämällä ```errno```-muuttujaa, mutta kirjoittaminen standardivirheeseen on yksi helpoimmista tavoista ilmoittaa virhe ohjelmassa.

## Katso myös:

- [fprintf-funktio](https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm)
- [errno-muuttuja](https://www.tutorialspoint.com/c_standard_library/c_macro_errno.htm)
- [virheenkäsittely C:ssä](https://www.geeksforgeeks.org/error-handling-c-programs/)