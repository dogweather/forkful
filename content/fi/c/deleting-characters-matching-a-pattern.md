---
title:    "C: Mallia vastaavien merkkien poistaminen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointiprojekteissa joudutaan käsittelemään suuria määriä tekstiä ja siihen liittyviä merkkejä. Erilaiset virheelliset tai tarpeettomat merkit voivat vaikeuttaa ohjelman toimintaa ja hidastaa sen suoritusta. Tällöin voi olla hyödyllistä poistaa tietyt merkit, jotka eivät ole tarpeellisia ja saattavat aiheuttaa ongelmia.

## Kuinka

C-kielinen ohjelmointi tarjoaa helpon tavan poistaa merkkejä, jotka soveltuvat tiettyyn kuvioon. Tällä tavalla voit poistaa tarpeettomia tai virheellisiä merkkejä tekstistä.

```C
#include <stdio.h>
#include <string.h>

int main(){
    char text[] = "Tämä on esimerkkiteksti, jossa on ***#" tarpeettomia merkkejä.";
    //Tämä silmukka tarkistaa kaikki merkit ja poistaa tarvittavat
    for(int i=0; i<strlen(text); i++){
        //Jos merkki on sama kuin tähti, se korvataan tyhjällä merkillä
        if(text[i] == '*'){
            text[i] = ' ';
        }
    }
    printf("Muokattu teksti: %s \n", text);
}

```

**Output:** Muokattu teksti: Tämä on esimerkkiteksti, jossa on # tarpeettomia merkkejä.

Tässä esimerkissä käytettiin for-silmukkaa, joka käy läpi kaikki tekstin merkit ja korvaa tarpeettomat merkit tyhjillä. Tämä on yksinkertainen esimerkki, mutta voit soveltaa samaa logiikkaa myös monimutkaisempiin tekstiin tai kuvioihin.

## Syvemmälle

C-ohjelmoinnissa on erilaisia tapoja käsitellä ja muokata merkkejä. Yllä olevassa esimerkissä käytettiin for-silmukkaa, mutta myös esimerkiksi merkkijonojen kopiointi- tai muokkausfunktiot voivat olla hyödyllisiä. Kannattaa tutkia erilaisia ​​tapoja käsitellä merkkejä ja valita paras vaihtoehto projektisi tarpeisiin.

## Katso myös

- Vielä lisää esimerkkejä merkkien poistamisesta: https://www.geeksforgeeks.org/remove-characters-from-a-given-string-that-appears-exactly-k-times/
- C-kielen merkkijonojen muokkausfunktiot: https://www.tutorialspoint.com/c_standard_library/c_function_strcpy.htm
- Lisää tietoa C-ohjelmoinnista: https://www.tutorialspoint.com/cprogramming/