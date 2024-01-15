---
title:                "Uuden projektin aloittaminen"
html_title:           "C++: Uuden projektin aloittaminen"
simple_title:         "Uuden projektin aloittaminen"
programming_language: "C++"
category:             "C++"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi aloittaa uusi projekti?
Uuden projektin aloittaminen voi olla hyvä tapa oppia uusia ohjelmointitaitoja, kehittää omaa osaamistaan ja luoda jotain mielenkiintoista ja hyödyllistä. Se voi myös tarjota mahdollisuuden työskennellä yhdessä muiden kehittäjien kanssa ja saada kokemusta projektinhallinnasta.

## Kuinka aloittaa uusi projekti?
Aloittaaksesi uuden projekti, sinun tulee ensin luoda uusi C++ -tiedosto ja määritellä tarvittavat kirjastot. Esimerkiksi:

```C++
#include <iostream>
#include <string>

using namespace std;

int main(){
    // koodin kirjoittaminen tähän
    return 0;
}
```

Tässä esimerkissä olemme ottaneet käyttöön "iostream" ja "string" -kirjastot, jotka mahdollistavat käyttäjän syötteiden lukemisen ja merkkijonojen käsittelyn. Voit käyttää muita tarvittavia kirjastoja projektisi tarpeiden mukaan. Seuraavaksi voit kirjoittaa koodin, joka suorittaa haluttuja toimintoja ja näyttää tulosteen. Esimerkiksi:

```C++
string nimi;
cout << "Hei! Mikä on nimesi?";
getline(cin, nimi);

cout << "Mukava tavata, " << nimi << "!" << endl;
```

Tässä koodissa käytämme "string" -muuttujaa tallentamaan käyttäjän antamaa nimeä ja tulostamme sitten tervehdyksen käyttäen tätä muuttujaa. Voit kokeilla erilaisia koodinpätkiä ja tutkia eri mahdollisuuksia C++:n avulla.

## Syvällisempää tietoa uuden projektin aloittamisesta
Yksi tärkeimmistä asioista C++ -projektin aloittamisessa on hyvän suunnitelman tekeminen. Helpointa on aloittaa pienellä ja yksinkertaisella projektilla, jonka avulla voit oppia perusteet ja laajentaa sitten taitojasi. Muista myös ottaa huomioon projektisi tarkoitus ja lopputavoitteet. On myös tärkeää pitää huolta koodisi luettavuudesta ja kommentoida sitä tarvittaessa.

## Katso myös
- [C++:n opetusohjelmat ja resurssit (Suomi)](https://www.cplusplus.com/doc/tutorial/)
- [Kuinka aloittaa uusi projekti C++:ssa (englanniksi)](https://www.digitalocean.com/community/tutorials/how-to-start-a-new-project-in-cplusplus)
- [Esimerkkejä C++ -projekteista (englanniksi)](https://www.codementor.io/projects/language/cpp)