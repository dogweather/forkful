---
title:                "Komentoriviparametrien lukeminen"
html_title:           "C++: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & miksi?
Lue komentorivin argumentteja tarkoittaa ohjelman lukemista ja hyödyntämistä niitä, jotka on annettu ohjelman käynnistämiseen liittyvässä komennossa. Tämä on hyödyllistä ohjelmoinnissa, jotta voidaan antaa erilaisia tietoja tai asetuksia, joita ohjelma tarvitsee suorittamiseen. Esimerkiksi voit antaa tiedoston nimen tai muita parametreja, jotka vaikuttavat ohjelman toimintaan.

## Kuinka:
### Esimerkki 1:
Tässä näytetään, kuinka voit lukea komentorivin argumentit ja tulostaa ne näytölle:

```C++
#include <iostream>
#include <string>

int main(int argc, char* argv[]) {
    // Tulostaa kaikki annetut argumentit
    for (int i = 0; i < argc; i++) {
        std::cout << "Argument #" << i << ": " << argv[i] << std::endl;
    }
    return 0;
}
```

### Esimerkki 2:
Voit myös käsitellä erikseen tiettyjä argumentteja, kuten tässä, jossa lukee syötetyn tiedoston nimen ja tulostaa sen sisällön:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main(int argc, char* argv[]) {
    std::string filename;
    
    // Tarkistetaan onko annettu tiedoston nimi
    if (argc > 1) {
        // Tiedoston nimi on annettu ensimmäisenä argumenttina
        filename = argv[1];
        
        // Luetaan tiedosto
        std::ifstream file(filename);
        
        // Tulostetaan tiedoston sisältämät rivit
        std::string line;
        while (getline(file, line)) {
            std::cout << line << std::endl;
        }
        file.close();
    }
    return 0;
}
```

Saatat saada tulosteen, kuten tämä:

```
Hello World!
This is an example file.
```

## Syväsukellus:
### Historiallinen konteksti:
Ennen kuin oli graafisia käyttöliittymiä, ohjelmia ajettiin komentoriviltä, ja komentorivin argumentit olivat tärkeä osa niiden käyttöä. Tämä ominaisuus on edelleen käytössä, vaikka graafiset käyttöliittymät ovat yleistyneet.

### Vaihtoehtoja:
Jos et halua lukea komentorivin argumentteja, voit myös käyttää ympäristömuuttujia tai lukea tiedot käyttäjältä suoraan ohjelman suorituksen aikana.

### Toteutus:
Komentorivin argumentit välitetään ohjelman käyttöjärjestelmän käynnistyskohdassa, ja ne tallennetaan pääohjelman argumenttiluetteloon. Tämän avulla ohjelma voi käyttää niitä suorituksen aikana.

## Katso myös:
- [Standard Linux command line arguments](https://tldp.org/LDP/abs/html/standard-options.html)
- [Command line arguments in C++](https://www.tutorialspoint.com/command-line-arguments-in-cplusplus)
- [Environment variables vs command line arguments](https://www.ctl.io/developers/blog/post/linux-commandline-arguments-vs-environment-variables/)