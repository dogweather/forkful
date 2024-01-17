---
title:                "Aloittamassa uutta projektia"
html_title:           "C++: Aloittamassa uutta projektia"
simple_title:         "Aloittamassa uutta projektia"
programming_language: "C++"
category:             "C++"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Uuden projektin aloittaminen on se, kun ohjelmoija luo uuden tietokoneohjelmiston. Tämä on usein tehtävä, kun halutaan luoda uusi sovellus tai ratkaista jokin ongelma. Ohjelmoijat aloittavat uusia projekteja, jotta he voivat luoda jotain uutta ja käyttää hyödyksi uusia teknologioita. 

## Kuinka:

Ohessa on esimerkki siitä, miten uusi projekti voidaan aloittaa käyttäen C++ ohjelmointikieltä:

```
// Suorakulmion pinta-ala laskuri
#include <iostream>

using namespace std;

int main() {
  // Määritellään suorakulmion mitat
  double pituus, leveys;
  cout << "Syötä suorakulmion pituus: ";
  cin >> pituus;
  cout << "Syötä suorakulmion leveys: ";
  cin >> leveys;

  // Lasketaan ja tulostetaan pinta-ala
  double pinta_ala = pituus * leveys;
  cout << "Suorakulmion pinta-ala on: " << pinta_ala << endl;

  return 0;
}
```
Tulostus:
```
Syötä suorakulmion pituus: 5
Syötä suorakulmion leveys: 3
Suorakulmion pinta-ala on: 15
```

## Syvällisempi sukellus:

Uusien projektien aloittamisella on pitkät perinteet ohjelmoinnin historiassa. Ohjelmoijat aloittivat uusia projekteja jo alusta lähtien, kun tietokoneet tulivat käyttöön. Tämä johtuu siitä, että ohjelmistot ja niiden teknologiat kehittyvät jatkuvasti ja uusia ratkaisuja tarvitaan. On myös olemassa monia muita vaihtoehtoja C++:lle, kuten Java, Python ja C#, joita voidaan käyttää uusien projektien aloittamiseen. Jokaisella kielellä on omat ominaisuutensa ja vahvuutensa, joten ohjelmoijien pitäisi valita käytettävä kieli sen perusteella, mikä sopii parhaiten heidän tarpeisiinsa ja projekteihinsa.

## Katso myös:

- [C++ ohjelmointikielen virallinen sivusto](https://isocpp.org/)
- [Ohjeet uuden projektin aloittamiseen C++:lla](https://www.edureka.co/blog/how-to-start-a-new-project-in-cpp/)
- [Vinkkejä ohjelmistoarkkitehdille uusien projektien suunnitteluun](https://www.archdaily.com/769198/how-to-start-a-new-project-architectural-project-planning-checklist)