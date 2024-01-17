---
title:                "Tulevan tai menneen päivämäärän laskeminen"
html_title:           "C: Tulevan tai menneen päivämäärän laskeminen"
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Mitä ja miksi?

Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä on prosessi, jossa ohjelma muuttaa annetun päivämäärän halutun ajanjakson verran eteen- tai taaksepäin. Tämä on hyödyllistä esimerkiksi silloin, kun haluamme laskea tietyn päivämäärän vaikka viikon tai kuukauden päähän. Ohjelmoijat tekevät tämän voidakseen automatisoida tällaisia laskentaprosesseja.

# Miten:

### Esimerkki 1: Päivän lisääminen annettuun päivämäärään

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t rawtime;
    struct tm *timeinfo;
    
    // Alustetaan aika nykyiseen kelloaikaan
    
    time(&rawtime);
    timeinfo = localtime(&rawtime);
    
    // Tulostetaan nykyinen aika
    
    printf("Nykyinen aika: %s", asctime(timeinfo));
    
    // Lisätään yksi päivä aikaan ja tulostetaan uusi aika
    
    timeinfo->tm_mday = timeinfo->tm_mday + 1;
    mktime(timeinfo);
    printf("Huomisen aika: %s", asctime(timeinfo));
    
    return 0;
}
```

**Tuloste:**

```
Nykyinen aika: Thu Jul 15 09:44:42 2021
Huomisen aika: Fri Jul 16 09:44:42 2021
```

### Esimerkki 2: Päivämäärän muuttaminen halutun ajan verran taaksepäin

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t rawtime;
    struct tm *timeinfo;
    
    // Alustetaan aika nykyiseen kelloaikaan
    
    time(&rawtime);
    timeinfo = localtime(&rawtime);
    
    // Tulostetaan nykyinen aika
    
    printf("Nykyinen aika: %s", asctime(timeinfo));
    
    // Vähennetään 2 kuukautta aikaan ja tulostetaan uusi aika
    
    timeinfo->tm_mon = timeinfo->tm_mon - 2;
    mktime(timeinfo);
    printf("Kaksi kuukautta sitten: %s", asctime(timeinfo));
    
    return 0;
}
```

**Tuloste:**

```
Nykyinen aika: Thu Jul 15 09:45:30 2021
Kaksi kuukautta sitten: Mon May 17 09:45:30 2021
```

# Syvempi sukellus:

Historiallinen konteksti: Päivämäärän laskeminen tulevaisuudessa tai menneisyydessä on tärkeä osa ohjelmistokehitystä ja tarpeen monissa sovelluksissa. Tästä löytyy useita eri tapoja ja algoritmeja, mutta C-kielessä käytämme yleensä ajanhallintakirjaston funktionaalisuutta (time.h).

Vaihtoehdot: Lisäksi C-kielessä on myös muita tapoja toteuttaa päivämäärän laskeminen, kuten käyttämällä päivämääräkäsittelyä mahdollistavia kirjastoja, kuten date.h tai jopa luomalla oma algoritmi. Myös eri ohjelmointikielillä voi olla omat ratkaisunsa tähän tarpeeseen.

Toteutusyksityiskohtia: C-kielessä käytämme yleensä mktime-funktiota, joka muuttaa annetun ajan rakenteen tavallisessa muodossa olevaksi aikamerkinnäksi (epoch). Tämä mahdollistaa päivämäärän laskemisen haluttuun suuntaan. On myös tärkeää huolehtia siitä, että ajan rakenteessa oikein asetetut arvot, kuten kuukaudet ja vuodet, jotta laskenta sujuu oikein.

# Katso myös:

- [Time and Date Functions in C](https://www.tutorialspoint.com/c_standard_library/c_function_time.htm)
- [Date and Time Functions in C](https://www.programiz.com/c-programming/library-function/time.h)
- [How to Subtract or Add to a Date in C](https://www.codeproject.com/Articles/1982/How-to-Subtract-or-Add-to-a-Date-in-C-Creation-and)