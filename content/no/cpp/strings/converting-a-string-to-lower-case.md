---
date: 2024-01-20 17:38:06.523323-07:00
description: "Omforming av strenger til sm\xE5 bokstaver betyr \xE5 konvertere alle\
  \ tegn i en streng til deres tilsvarende sm\xE5 bokstavform. Programmerere gj\xF8\
  r dette for data-\u2026"
lastmod: '2024-03-13T22:44:41.086489-06:00'
model: gpt-4-1106-preview
summary: "Omforming av strenger til sm\xE5 bokstaver betyr \xE5 konvertere alle tegn\
  \ i en streng til deres tilsvarende sm\xE5 bokstavform. Programmerere gj\xF8r dette\
  \ for data-\u2026"
title: "Konvertere en streng til sm\xE5 bokstaver"
weight: 4
---

## What & Why?
Omforming av strenger til små bokstaver betyr å konvertere alle tegn i en streng til deres tilsvarende små bokstavform. Programmerere gjør dette for data-normalisering, å forenkle sammenligninger og søk, eller å oppfylle brukerkrav.

## How to:
Her er den kjappe metoden for å endre store bokstaver til små i C++:

```C++
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string tekst = "Hei Verden!";
    std::transform(tekst.begin(), tekst.end(), tekst.begin(), 
                   [](unsigned char c){ return std::tolower(c); });
    
    std::cout << tekst << std::endl; // Output: hei verden!
    return 0;
}
```

Dette bruker `<algorithm>` biblioteket for å tranformere hver karakter ved hjelp av `std::tolower`.

## Deep Dive
Før C++11, kunne man bruke løkker, C-funksjoner eller egne implementasjoner for å oppnå det samme. Fra C++11 og fremover gir standardbiblioteket rikere funksjonalitet for slike oppgaver. 

Alternativer inkluderer å bruke `<cctype>` funksjoner som `tolower()` per karakter i en for-loop:

```C++
for (auto &c : tekst) {
    c = tolower(c);
}
```

Når vi snakker om implementasjonsdetaljer, er det verd å merke seg at `std::tolower` er lokaliseringssensitiv, som betyr at den baserer seg på programmet sitt innstilte lokalitetssystem. For ASCII-tegn, er dette vanligvis ikke en bekymring. Men for utvidede ASCII-sett eller Unicode-tegn, kan dette bli komplekst.

`std::transform` og `std::tolower` er ofte brukt fordi de er enkle, rett-til-punktet løsninger, men de kan ha begrensninger med unicode eller multibyte tegnsett hvor en mer robust løsning som ICU (International Components for Unicode) kan være mer hensiktsmessig.

## See Also
- C++ Standard Library documentation: https://en.cppreference.com/w/
- International Components for Unicode (ICU): http://site.icu-project.org/
- Utfyllende informasjon om C++ Lokalisering: https://en.cppreference.com/w/cpp/locale
