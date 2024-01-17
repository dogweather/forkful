---
title:                "Kahden päivämäärän vertailu"
html_title:           "C++: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Vertaillessa kahta eri päivämäärää ohjelmoijat tarkistavat, ovatko päivämäärät samat vai erilaiset. Tämä on tärkeää esimerkiksi kun halutaan tarkistaa, onko tietty tapahtuma tapahtunut ennen vai jälkeen tiettyä päivää.

Asioita voi vertailla monella eri tavalla kaikkien mahdollisten laskutoimitusten avulla, mutta tiettyjen tietueiden käyttö helpottaa prosessia ja tekee sen luotettavammaksi.

## Kuinka: 
Vertaillaan kahta päivämäärää: 01 toukokuuta 2021 ja 15 kesäkuuta 2021.
```C++
#include <iostream>
#include <ctime>
using namespace std;

// Funktio vertailee kahta annettua päivämäärää
// Palauttaa true, jos päivämäärät ovat samat
bool vertailePäivämääriä(int pv1, int kk1, int v1, int pv2, int k2, int v2)
{
    if (pv1 == pv2 && kk1 == k2 && v1 == v2)
        return true;
    return false;
}

int main()
{
    int pv1 = 1; // Ensimmäisen päivämäärän päivä
    int kk1 = 5; // Ensimmäisen päivämäärän kuukausi
    int v1 = 2021; // Ensimmäisen päivämäärän vuosi

    int pv2 = 15; // Toinen päivämäärä
    int kk2 = 6;
    int v2 = 2021;

    if (vertailePäivämääriä(pv1, kk1, v1, pv2, kk2, v2))
        cout << "Päivämäärät ovat samat.";
    else
        cout << "Päivämäärät ovat erilaiset.";
    return 0;
}
```

Output:
```
Päivämäärät ovat erilaiset.
```

## Syväsukellus:
Varhaisemmissa ohjelmissa päivämääriä vertailtiin usein yhdistämällä ne yhteen merkkijonoon ja vertailemalla sen avulla. Tämä tapa ei ollut kovin käytännöllinen, sillä se aiheutti usein ongelmia eri muodoissa olevien päivämäärien kanssa.

Nykyisin monet kielet tarjoavat valmiita tietueita päivämäärien käsittelyä varten, mikä tekee vertailun helpommaksi ja luotettavammaksi. Vaihtoehtoisesti voidaan myös käyttää erilaisia päivämääräkirjastoja, jotka tarjoavat erilaisia toimintoja ja tarkemman kontrollin päivämäärien vertailuun.

Päivämäärien vertailussa tarkastellaan yleensä päivä-, kuukausi- ja vuosilukuja. Lisäksi on tärkeää huomioida esimerkiksi karkausvuodet ja eri aikavyöhykkeet, jotta vertailu on mahdollisimman tarkka ja tehokas.

## Katso myös:
- [cppreference.com - Date and time library](https://en.cppreference.com/w/cpp/chrono)
- [GeeksforGeeks - Comparing dates in C++](https://www.geeksforgeeks.org/comparing-dates-cpp/)