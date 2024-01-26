---
title:                "Lokitus"
date:                  2024-01-26T01:01:40.649673-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lokitus"
programming_language: "C++"
category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/logging.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Ohjelmoinnin yhteydessä lokiin kirjaaminen tarkoittaa tapahtumien, tilojen ja tiedon tallentamista tiedostoon tai muuhun ulostuloalustaan. Ohjelmoijat kirjaavat lokiin pysyäkseen kärryillä siitä, mitä heidän sovelluksissaan tapahtuu, debugatakseen ongelmia ja seuratakseen suorituskykyä tulevaa analyysia ja optimointia varten.

## Kuinka:
Oletetaan, että työskentelet Linux-koneella ja haluat dumpata lokitiedostot tiedostoon käyttäen hyvää vanhaa C++. Haluat sisällyttää `<iostream>`- ja `<fstream>`-kirjastot tiedosto-operaatioita varten. Tässä on nopea esimerkki:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream logFile("appLog.txt", std::ios::app);  // Avaa lisäystilassa

    if (!logFile.is_open()) {
        std::cerr << "Lokitiedoston avaamisessa oli ongelma!" << std::endl;
        return 1;
    }

    logFile << "Sovellus käynnistetty" << std::endl;
  
    // ... jossain sovelluksesi logiikassa
    logFile << "Tärkeä tapahtuma on sattunut" << std::endl;

    // Älä unohda sulkea tiedostovirtaasi
    logFile.close();

    return 0;
}
```

Jos seuraat lokitiedostoa komennolla `tail -f appLog.txt`, pitäisi näkyä:

```
Sovellus käynnistetty
Tärkeä tapahtuma on sattunut
```

Siistiä, sinulla on aikaleimattu tapahtumarekisteri!

## Syvä Sukellus
Lokiin kirjaaminen on yhtä vanhaa kuin tietotekniikka itse, juontuen aidoista merkeistä paperilla, joiden avulla jäljitettiin muinaisten tietokoneiden toimintaa. Nykyajan ohjelmointimaailmassa kaikki on kyse monimutkaisista ohjelmistoratkaisuista. On suoraa-tiedostoon kirjaavaa lokitusta, kuten yllä oleva nopea ja likainen esimerkki, tai voit hemmotella itseäsi hienostuneemmalla lokituskehyksellä, kuten Log4cpp tai Boost.Log C++ alueella; nämä pahat pojat tarjoavat lokitustasoja, formaatin hallintaa ja enemmän.

Puhuttaessa tasoista, lokituskäytäntöjen parhaisiin tapoihin kuuluu eri vakavuustasojen käyttäminen – info, debug, varoitus, virhe, kohtalokas – jotta voit suodattaa kohinan, kun yrität murskata bugeja tai selvittää, miksi sovelluksesi käyttäytyy kuin oikukas teini.

Suorituskykynoteerauksena, älä lähde löysäilemään lokien kanssa. Liiallinen lokitus voi muuttaa salamannopean sovelluksesi etanan maratoniksi, hidastaa tiedostojärjestelmiä tai jopa tulla sinulle kalliiksi tallennusmaksuina, jos olet pilvipohjainen. Oikean tasapainon löytäminen on avain: kirjaa mitä tarvitset, eikä yhtään enempää.

## Katso Myös
Niille teistä, jotka haluavat mennä pidemmälle lokituskäytäntöjen kanssa, katsokaa näitä:

- [Boost.Log-kirjasto](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html) joitakin raskaita lokitusominaisuuksia varten.
- [Googlen glog-kirjasto](https://github.com/google/glog), jos olet kiinnostunut siitä, mitä teknologiajätin kokit käyttävät sovellustensa lokitukseen.
- [Log4cpp-kirjasto](http://log4cpp.sourceforge.net/) mukautettavaan lokitusmekanismiin.

Ja taustatietoja lokituksesta miksi ja miten, syventykää:

- Tämä Stack Overflow -ketju [lokitusparhaista käytännöistä](https://stackoverflow.com/questions/783956/logging-best-practices) antaa vertaisarvioidun syväsukelluksen aiheeseen.