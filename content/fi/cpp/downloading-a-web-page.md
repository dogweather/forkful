---
title:                "C++: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi

Web-sivujen lataaminen on tärkeä osa web-kehitystä ja ohjelmointia. Se antaa meille mahdollisuuden saada tietoa erilaisista verkkosivustoista ja käyttää niiden tietoja jatkossa. Esimerkiksi voimme luoda web-sovelluksia, jotka käyttävät verkkosivustojen tietoja tärkeänä osana niiden toimintaa.

## Miten

Web-sivujen lataaminen voidaan toteuttaa monella eri tavalla käyttäen erilaisia ohjelmointikieliä ja tekniikoita. Tässä blogikirjoituksessa keskitymme siihen, miten voit ladata web-sivuja käyttäen C++-kieltä.

Alla olevissa esimerkeissä käyttämämme koodin avulla voit ladata web-sivun ja tulostaa sen sisällön konsolille. Huomaa, että tarvitset lisäksi C++:n sisäänrakennetun "urlmon.h"-kirjaston, jotta voit käyttää esimerkkikoodia.

```C++

#include <iostream>
#include <urlmon.h>

using namespace std;

int main()
{
    // Muuta haluamasi web-sivun osoite alle
    string url = "https://www.example.com";

    // Luo muuttuja ladataksesi web-sivun sisällön
    string pageContent;

    // Käytä URLDownloadToFile-funktiota lataamaan web-sivun sisältö
    HRESULT result = URLDownloadToFile(NULL, url.c_str(), "output.html", 0, NULL);

    // Tarkista, että lataus onnistui
    if (result == S_OK)
    {
        cout << "Web-sivu ladattu!" << endl;
        
        // Siirry ladatun sivun sisältö muuttujaan
        pageContent = "output.html";
        
        // Tulosta sivun sisältö konsolille
        cout << pageContent << endl;
    }
    else
    {
        cout << "Web-sivun lataus epäonnistui." << endl;
    }

    return 0;
}

```

## Syväsukellus

Web-sivujen lataaminen tapahtuu HTTP-protokollan avulla. Ohjelmassamme käytämme "URLDownloadToFile"-funktiota, joka on osa "urlmon.h"-kirjastoa ja joka hoitaa web-sivun lataamisen ja tallentamisen haluttuun tiedostoon.

Ensimmäisenä argumenttina URLDownloadToFile-funktiolle annetaan "NULL", joka tarkoittaa, että funktio käytetään synkronisesti. Toisena argumenttina annetaan web-sivun osoite ja kolmantena tiedostonimi, johon sivun sisältö tallennetaan. Neljäntenä argumenttina annetaan "0", joka tarkoittaa, että lataus tapahtuu oletusarvoisella tavalla. Viimeisenä argumenttina annetaan "NULL".

## Katso myös

- [URLDownloadToFile-funktio (Microsoft Docs)](https://docs.microsoft.com/en-us/windows/win32/api/urlmon/nf-urlmon-urldownloadtofilea)
- [HTTP-protokolla (Mozilla Developer Network)](https://developer.mozilla.org/en-US/docs/Web/HTTP)