---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstitiedostojen kirjoittaminen tarkoittaa datan tallentamista luettavaan muotoon tiedostoon. Ohjelmoijat kirjoittavat tekstitiedostoja, koska se on helppo tapa tallentaa ja jakaa tietoa.

## How to:
```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream outFile("esimerkki.txt");
    if (outFile.is_open()) {
        outFile << "Hei, tämä on tekstirivi tiedostossa.\n";
        outFile << "Toinenkin rivi tekstiä.\n";
        outFile.close();
    } else {
        std::cerr << "Tiedoston avaus epäonnistui.\n";
    }
    return 0;
}
```
Sample output in "esimerkki.txt":
```
Hei, tämä on tekstirivi tiedostossa.
Toinenkin rivi tekstiä.
```

## Deep Dive
Alun perin C++ käytti C:n FILE* osoittimia tiedoston käsittelyyn. `std::ofstream` tuli myöhemmin C++:n standardikirjastoon parempana työkaluna tiedostojen kirjoittamiseen. `ofstream` on osa "fstream"-kirjastoa (file stream), ja se tukee RAII-periaatetta, joka automatisoi resurssien hallinnan. Vaihtoehtoisesti voitaisiin käyttää moderneja C++17-filesystem-kirjaston toimintoja tai vanhempia C:n "stdio.h"-tyyppisiä funktioita.

## See Also
- C++ Standard Library Documentation: [http://www.cplusplus.com/reference/fstream/ofstream/](http://www.cplusplus.com/reference/fstream/ofstream/)
- C++ Filesystem Library: [https://en.cppreference.com/w/cpp/filesystem](https://en.cppreference.com/w/cpp/filesystem)
- C I/O Functions Tutorial: [https://www.tutorialspoint.com/cprogramming/c_input_output.htm](https://www.tutorialspoint.com/cprogramming/c_input_output.htm)
