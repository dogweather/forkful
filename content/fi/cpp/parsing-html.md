---
title:                "HTML: n jäsentäminen"
html_title:           "C++: HTML: n jäsentäminen"
simple_title:         "HTML: n jäsentäminen"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi & Mitä?
HTML:n parsiminen tarkoittaa verkkosivun koodin lukemista ja sen tietojen erottamista käyttökelpoisiksi tietorakenteiksi. Ohjelmoijat käyttävät parsimista tehdäkseen verkkosivujen sisällöstä helposti käsiteltävää ja analysoitavaa dataa.

## Miten:
```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    // Luodaan merkkijono esimerkiksi HTML-koodille
    string html = "<html><head><title>Esimerkki sivu</title></head><body><h1>Tämä on otsikko</h1><p>Tämä on tekstiä</p></body></html>";

    // Tulostetaan otsikko
    size_t start = html.find("<h1>") + 4; // Etsitään otsikon alku
    size_t end = html.find("</h1>"); // Etsitään otsikon loppu
    cout << "Otsikko: " << html.substr(start, end-start) << endl;

    // Tulostetaan tekstit
    start = html.find("<p>") + 3; // Etsitään tekstin alku
    end = html.find("</p>"); // Etsitään tekstin loppu
    cout << "Teksti: " << html.substr(start, end-start) << endl;

    return 0;
}
```

Lähtö:
```
Otsikko: Tämä on otsikko
Teksti: Tämä on tekstiä
```

## Syvempi sukellus:
HTML:n parsiminen kehitettiin 1990-luvulla, kun Internet-käyttö yleistyi. Tähän mennessä on kehitetty useita eri tapoja parsia HTML:ää, joista yksi on regulääri ilmeisyyksien avulla. Tämä kuitenkin ei ole paras vaihtoehto, sillä HTML:n rakenteet voivat olla hyvin monimutkaisia ja sen parsiminen on vaativaa. Sen sijaan käytetään usein parseereita, jotka ovat erityisesti HTML:n parsimiseen suunniteltuja ohjelmia.

## Katso myös:
- [Mozilla Developer Network: Introduction to HTML](https://developer.mozilla.org/en-US/docs/Learn/HTML/Introduction_to_HTML)