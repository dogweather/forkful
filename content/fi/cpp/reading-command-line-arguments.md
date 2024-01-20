---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Elm: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?
Komennorivin argumenttien lukeminen tarkoittaa tulotietojen noutamista pääohjelmaan komennoriviltä. Tämä on hyödyllistä parametrien välittämiseen ohjelmallesi, ja se mahdollistaa joustavat ja dynaamiset ohjelmat.

# Kuinka:
C++:lla pääset käsiksi komennorivin argumentteihin pääfunktion parametrien avulla. Tässä esimerkki:

```C++
#include <iostream>

int main(int argc, char *argv[]) {
    for (int i = 0; i < argc; ++i) {
        std::cout << "Argumentti " << i << " : " << argv[i] << std::endl;
    }
    return 0;
}
```

Jos ohjelma suoritetaan kommentilla `program esimerkki1 esimerkki2`, tuloste on:

```
Argumentti 0 : program
Argumentti 1 : esimerkki1
Argumentti 2 : esimerkki2
```

# Syvemmälle
Komennorivin argumenttien käyttö ei ole uusi käsite, se on ollut olemassa vuosikymmenet. Se tarjoaa tehokkuutta ja joustavuutta ohjelmointiin. Vaihtoehtona on kovakoodausten käyttäminen tai datan ottaminen tiedostoista, joka voi johtaa monimutkaisempaan ja vähemmän mukautettavaan koodiin.

Pääfunktiossa `argc` on argumenttien määrä, `argv` on argumenttien arvot. Huomaa, että argv[0] on aina ohjelman suoritettavan tiedoston nimi.

# Katso Myös
- C++ Komennorivin Argumentit (Kattavampi opas): https://www.sanfoundry.com/cpp-programming-examples-command-line-arguments/
- C++:n Virallinen Dokumentaatio: https://en.cppreference.com/w/cpp/language/main_function