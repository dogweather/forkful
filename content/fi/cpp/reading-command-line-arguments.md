---
title:                "Komennoriviparametrien lukeminen"
date:                  2024-01-20T17:55:42.729081-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja miksi?)
Komennon rivin argumentit ovat parametreja, jotka annetaan ohjelmalle sen käynnistyessä. Ne mahdollistavat joustavan ohjelman käytön, koska käyttäjä voi määrittää toimintoja ohjelman ulkopuolelta.

## How to: (Kuinka tehdä:)
```C++
#include <iostream>

int main(int argc, char *argv[]) {
    std::cout << "Ohjelmaan annettiin " << argc << " argumenttia.\n";
    
    for (int i = 0; i < argc; ++i) {
        std::cout << "Argumentti " << i << ": " << argv[i] << "\n";
    }
    
    return 0;
}

/* Käytä ohjelmaa komentoriviltä näin:
   $ ./ohjelma ensimmäinen toinen "kolmas neljäs"
   Tulos:
   Ohjelmaan annettiin 4 argumenttia.
   Argumentti 0: ./ohjelma
   Argumentti 1: ensimmäinen
   Argumentti 2: toinen
   Argumentti 3: kolmas neljäs
*/
```

## Deep Dive (Syväsukellus):
Komennon rivin argumentit ovat olleet osa ohjelmointia siitä asti, kun ihmiset alkoivat kirjoittaa monikäyttöisiä ohjelmia. C ja C++ tukevat näitä suoraan `main`-funktion kautta. Vaihtoehtoisia tapoja lukea argumentteja ovat kirjastot, kuten Boost.Program_options tai TCLAP, jotka tarjoavat monimutkaisempia toiminnallisuuksia. Tärkeää on ymmärtää, että `argv[0]` sisältää ohjelman nimen tai polun, ja laskenta alkaa tästä. Tietoturvasyistä on hyvä validoida ja desinfioida käyttäjän syöttämät komennot ennen niiden käyttöä.

## See Also (Katso myös):
- C++17 Standard (ISO/IEC 14882:2017): https://www.iso.org/standard/68564.html
- Boost.Program_options: https://www.boost.org/doc/libs/1_76_0/doc/html/program_options.html
- TCLAP - Templatized C++ Command Line Parser Library: http://tclap.sourceforge.net/
- Command-Line Arguments in C/C++: https://www.cprogramming.com/tutorial/command-line-arguments.html
