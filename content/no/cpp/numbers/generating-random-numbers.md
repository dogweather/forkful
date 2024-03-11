---
date: 2024-01-27 20:32:42.252909-07:00
description: "Generering av tilfeldige tall i programmering inneb\xE6rer \xE5 skape\
  \ sekvenser av tall som mangler forutsigbar rekkef\xF8lge eller m\xF8nster. Programmerere\
  \ bruker\u2026"
lastmod: '2024-03-11T00:14:14.688518-06:00'
model: gpt-4-0125-preview
summary: "Generering av tilfeldige tall i programmering inneb\xE6rer \xE5 skape sekvenser\
  \ av tall som mangler forutsigbar rekkef\xF8lge eller m\xF8nster. Programmerere\
  \ bruker\u2026"
title: Generering av tilfeldige tall
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Generering av tilfeldige tall i programmering innebærer å skape sekvenser av tall som mangler forutsigbar rekkefølge eller mønster. Programmerere bruker ofte disse tallene til ulike formål, som å simulere uforutsigbare hendelser, i testing og feilsøking, og i spillalgoritmer for å sikre rettferdighet eller uforutsigbarhet.

## Hvordan:

For å generere tilfeldige tall i C++, ville du typisk bruke `<random>`-biblioteket, som ble introdusert i C++11, og som tilbyr et bredt spekter av fasiliteter for å generere tilfeldige tall fra forskjellige distribusjoner.

```C++
#include <iostream>
#include <random>

int main() {
    // Initialiser en tilfeldig motor
    std::random_device rd;  
    std::mt19937 gen(rd()); 

    // Definer området [0, 99] inklusivt
    std::uniform_int_distribution<> distrib(0, 99); 

    // Generer og skriv ut 5 tilfeldige tall innenfor det definerte området
    for(int n=0; n<5; ++n)
        std::cout << distrib(gen) << ' ';
    return 0;
}
```

Dette kodeeksemplet initialiserer en Mersenne Twister-tilfeldig tallgenerator med et frø fra `std::random_device`. Det definerer deretter en jevn hel-tallsdistribusjon i området [0, 99] og skriver til slutt ut 5 tilfeldige tall fra denne distribusjonen.

Eksempelresultat kan se slik ut, men husk at hver utførelse sannsynligvis vil produsere forskjellige resultater:

```
45 67 32 23 88
```

## Dykk dypere:

Historisk sett har generering av tilfeldige tall i C++ lagt stor vekt på `rand()`-funksjonen og `srand()`-funksjonen for seeding, som finnes i `<cstdlib>`-biblioteket. Imidlertid har denne tilnærmingen ofte blitt kritisert for sin mangel på uniformitet og forutsigbarhet i distribusjonen av genererte tall.

Introduksjonen av `<random>`-biblioteket i C++11 markerte en betydelig forbedring og tilbyr et sofistikert system for produksjon av tilfeldige tall. Fasilitetene som tilbys inkluderer et mangfold av motorer (som `std::mt19937` for Mersenne Twister) og distribusjoner (som `std::uniform_int_distribution` for jevn distribusjon av heltall) som kan kombineres for å passe programmererens spesifikke behov, noe som fører til mer forutsigbar oppførsel, bedre ytelse og større fleksibilitet.

Selv om `<random>`-biblioteket er mye bedre enn den eldre `rand()`-tilnærmingen, er det verdt å merke seg at generering av virkelig tilfeldige tall—spesielt for kryptografiske formål—fortsatt krever ekstra vurderinger. For kryptografiske applikasjoner bør biblioteker som er spesielt designet for sikkerhet, og som ofte bruker maskinvareentropikilder, brukes i stedet.
