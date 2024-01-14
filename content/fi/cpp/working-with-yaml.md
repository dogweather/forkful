---
title:                "C++: Työskentely yaml:n kanssa."
simple_title:         "Työskentely yaml:n kanssa."
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi

YAML on tullut suosituksi tiedostonmuotojen käsittelyssä, koska se on helposti luettavaa ja ymmärrettävää tietokoneille ja ihmisille. Se on myös suosittu valinta monissa moderneissa sovelluksissa, kuten Kubernetes ja Docker, koska se tarjoaa helppokäyttöisen ja joustavan tavan tallentaa ja jakaa tietoa. Joten jos haluat työskennellä näiden suosittujen tekniikoiden kanssa, on tärkeää oppia käyttämään YAMLia.

## Kuinka

YAML-tiedostojen käsittely C++ -ohjelmoinnissa on helppoa ja vaivatonta. Voit käyttää YAML-CPP -kirjastoa, joka on suunniteltu nimenomaan YAML-tiedostojen käsittelyyn. Se tarjoaa käyttäjäystävällisen rajapinnan ja mahdollistaa tiedon tallentamisen ja lataamisen YAML-muodossa. Tässä on esimerkki koodista, joka lataa YAML-tiedoston ja tulostaa sen sisällön näytölle:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>

int main() {
    // Lataa YAML-tiedosto
    YAML::Node node = YAML::LoadFile("tiedosto.yaml");

    // Tulosta tiedoston sisältö näytölle
    std::cout << "Tiedoston sisältö:" << std::endl;
    std::cout << node << std::endl;

    return 0;
}
```

Kun suoritat tämän koodin, näet YAML-tiedoston sisällön näytöllä seuraavanlaisesti:

```
Tiedoston sisältö:
name: John
age: 25
city: Helsinki
```

## Syvempää perehtymistä

YAML-CPP tarjoaa myös monia muita toimintoja, kuten tietojen muokkaamisen ja tallentamisen takaisin YAML-muotoon. Voit myös luoda ja muokata YAML-tiedostoja suoraan koodissa käyttämällä YAML::Emitter ja YAML::Parser luokkia. Vaikka YAML-tiedostojen käsittely C++:lla voi olla hieman hankalaa alussa, se tarjoaa lopulta tehokkaan ja luotettavan tavan käsitellä tietoa sovelluksissasi.

## Katso myös

- [YAML-CPP:n dokumentaatio (englanniksi)](https://github.com/jbeder/yaml-cpp/wiki)
- [YAML-opas (englanniksi)](https://yaml.org/spec/1.2/spec.html)