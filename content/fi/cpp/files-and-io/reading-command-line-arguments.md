---
date: 2024-01-20 17:55:42.729081-07:00
description: "Komennon rivin argumentit ovat parametreja, jotka annetaan ohjelmalle\
  \ sen k\xE4ynnistyess\xE4. Ne mahdollistavat joustavan ohjelman k\xE4yt\xF6n, koska\
  \ k\xE4ytt\xE4j\xE4 voi\u2026"
lastmod: '2024-03-11T00:14:30.918944-06:00'
model: gpt-4-1106-preview
summary: "Komennon rivin argumentit ovat parametreja, jotka annetaan ohjelmalle sen\
  \ k\xE4ynnistyess\xE4. Ne mahdollistavat joustavan ohjelman k\xE4yt\xF6n, koska\
  \ k\xE4ytt\xE4j\xE4 voi\u2026"
title: Komennoriviparametrien lukeminen
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
