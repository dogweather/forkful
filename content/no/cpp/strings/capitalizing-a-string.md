---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:10.353005-07:00
description: "Det \xE5 kapitalisere en tekststreng inneb\xE6rer \xE5 konvertere det\
  \ f\xF8rste tegnet i hvert ord i strengen til store bokstaver hvis det er i sm\xE5\
  \ bokstaver,\u2026"
lastmod: '2024-03-11T00:14:14.675166-06:00'
model: gpt-4-0125-preview
summary: "Det \xE5 kapitalisere en tekststreng inneb\xE6rer \xE5 konvertere det f\xF8\
  rste tegnet i hvert ord i strengen til store bokstaver hvis det er i sm\xE5 bokstaver,\u2026"
title: Sette stor bokstav i en streng
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Det å kapitalisere en tekststreng innebærer å konvertere det første tegnet i hvert ord i strengen til store bokstaver hvis det er i små bokstaver, samtidig som de gjenværende tegnene forblir uendret. Programmerere utfører ofte denne oppgaven for formatering av utdata, brukerinndata eller databehandling for å sikre konsistens i hvordan tekst presenteres eller behandles, spesielt i brukergrensesnitt eller oppgaver for datanormalisering.

## Hvordan:
I C++ kan du kapitalisere en tekststreng ved å bruke standardbiblioteket uten behov for tredjepartsbiblioteker. Men, for mer komplekse eller spesifikke kapitaliseringsatferder, kan biblioteker som Boost være ganske nyttige. Nedenfor er eksempler som illustrerer begge tilnærminger.

### Ved bruk av standard C++-biblioteket:

```cpp
#include <iostream>
#include <cctype> // for std::tolower and std::toupper
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // Utdata: "Hello World From C++"
}
```

### Ved bruk av Boost-biblioteket:

For mer avansert strengmanipulasjon, inkludert lokaltilpasset kapitalisering, kan du ønske å bruke Boost String Algo-biblioteket.

Først, sørg for at du har installert og konfigurert Boost-biblioteket i prosjektet ditt. Deretter kan du inkludere de nødvendige headerne og bruke funksjonene som vist nedenfor.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // kapitaliser første bokstav i hvert ord
    boost::algorithm::to_lower(capitalizedText); // sikrer at strengen er i små bokstaver
    capitalizedText[0] = std::toupper(capitalizedText[0]); // kapitaliser første tegn

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // kapitaliser etter et mellomrom
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // Utdata: "Hello World From C++"
}
```

I dette tilfellet forenkler Boost noen av strengmanipulasjonsoppgavene, men krever fortsatt en tilpasset tilnærming for ekte kapitalisering siden det hovedsakelig tilbyr transformasjons- og bokstavkonverteringsverktøy.
