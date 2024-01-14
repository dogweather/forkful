---
title:                "C: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Ohjelmoinnin yksi tärkeimmistä tehtävistä on tekstin etsiminen ja korvaaminen. Se voi tuntua yksinkertaiselta tehtävältä, mutta se on erittäin tärkeää tekstipohjaisessa ohjelmoinnissa. Kun haluat muuttaa tiettyä sanaa tai lausetta kaikissa tiedostoissa tai korjata virheellisiä tietoja, tekstinhaku ja korvaaminen ovat välttämättömiä työkaluja tämän saavuttamiseen.

## Kuinka

Tekstin etsiminen ja korvaaminen on yleinen tehtävä C-ohjelmointikielellä ja se voidaan suorittaa muutamalla yksinkertaisella vaiheella. Ensinnäkin meidän täytyy avata tiedosto, jossa haluamme suorittaa etsimisen ja korvaamisen. Tämän jälkeen käytämme "fscanf" komentoa lukeaksemme tiedoston sisältöön.

**Esimerkki:**

```C
FILE *tiedosto;
char teksti[100];

tiedosto = fopen("tekstitiedosto.txt", "r");  
scanf(tiedosto, "%s", teksti);  
```

Tässä esimerkissä olemme avanneet "tekstitiedosto.txt" tiedoston ja tallentaneet sen sisällön muuttujaan "teksti". Jotta voimme suorittaa haun ja korvauksen, meidän täytyy käyttää tekstinhakufunktiota, kuten "strstr". Tämä funktio etsii annetun merkkijonon ja palauttaa löydettyjen merkkijonojen määrän.

**Esimerkki:**

```C
char uusi_teksti[100];

while (fgets(uusi_teksti, 100, tiedosto) != NULL) {  
    if (strstr(uusi_teksti, "vanha_sana")) {  
        strcpy(uusi_teksti, "uusi_sana");
    }  
} 

```

Tässä esimerkissä olemme käyttäneet "strstr" -funktiota etsimään ja korvaamaan "vanha_sana" merkkijonon "uusi_sana" -merkkijonolla. Lopuksi, kun olemme suorittaneet haun ja korvauksen kaikissa tiedoston merkkijonoissa, meidän täytyy tallentaa muutokset ja sulkea tiedosto.

**Esimerkki:**

```C
fclose(tiedosto);
```

## Syvällinen sukellus

Vaikka tekstinhaku ja korvaaminen ovat tärkeitä ohjelmoinnin tehtäviä, niiden tehokas suorittaminen vaatii hieman ymmärrystä C-kielen toiminnoista ja muuttujista. Esimerkiksi "fgets" -toiminnon käyttö tekstinhakuun voi olla hyödyllistä, kun etsitään merkkijonoja tiedostosta. Lisäksi "strcmp" -funktio voidaan käyttää vertaamaan kahta merkkijonoa ja varmistamaan, että korvaaminen tapahtuu vain silloin, kun merkkijonot ovat identtisiä.

On myös tärkeää varmistaa, että tiedosto suljetaan oikein suorituksen jälkeen. Jos tiedostoa ei suljeta, se voi aiheuttaa ongelmia myöhemmin.

## Katso myös

- [strcpy funktion dokumentointi](https://www.tutorialspoint.com/c_standard_library/c_function_strcpy.htm)
- [C:n muuttujat ja niiden tyypit](https://www.tutorialspoint.com/cprogramming/c_variables.htm)
- [Tekstitiedoston käsittely C-ohjelmoinnissa](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)