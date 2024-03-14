---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:20.576425-07:00
description: "Assosiative tabeller, kjent som `std::map` eller `std::unordered_map`\
  \ i C++, brobygger gapet mellom tabellindekser og virkelige data, og lar deg bruke\u2026"
lastmod: '2024-03-13T22:44:41.092309-06:00'
model: gpt-4-0125-preview
summary: "Assosiative tabeller, kjent som `std::map` eller `std::unordered_map` i\
  \ C++, brobygger gapet mellom tabellindekser og virkelige data, og lar deg bruke\u2026"
title: Bruke associative tabeller
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative tabeller, kjent som `std::map` eller `std::unordered_map` i C++, brobygger gapet mellom tabellindekser og virkelige data, og lar deg bruke meningsfulle nøkler. De er det foretrukne valget når du trenger raske oppslag, innsettinger og slettinger ved bruk av nøkler i stedet for indeksposisjoner.

## Hvordan:

I C++ blir assosiative tabeller levendegjort med hodefilene `<map>` og `<unordered_map>`. La oss ta noen eksempler for å se begge i aksjon.

### Bruk av `std::map`

`std::map` holder elementer sortert basert på nøkkelen. Slik kommer du i gang:

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> aldersMap;
    
    // Innsetting av verdier
    aldersMap["Alice"] = 30;
    aldersMap["Bob"] = 25;
    
    // Tilgang til verdier
    std::cout << "Bobs alder: " << aldersMap["Bob"] << std::endl;
    
    // Iterere over en map
    for(const auto &par : aldersMap) {
        std::cout << par.first << " er " << par.second << " år gammel." << std::endl;
    }
    
    return 0;
}
```

### Bruk av `std::unordered_map`

Når rekkefølge ikke spiller noen rolle, men ytelsen gjør det, er `std::unordered_map` din venn, som tilbyr raskere gjennomsnittlig kompleksitet for innsettinger, oppslag og slettinger.

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> produktPris;
    
    // Innsetting av verdier
    produktPris["melk"] = 2.99;
    produktPris["brød"] = 1.99;
    
    // Tilgang til verdier
    std::cout << "Melkepris: $" << produktPris["melk"] << std::endl;
    
    // Iterere over et unordered_map
    for(const auto &par : produktPris) {
        std::cout << par.first << " koster $" << par.second << std::endl;
    }
    
    return 0;
}
```

## Dykk dypere

Assosiative tabeller i C++, spesielt `std::map` og `std::unordered_map`, handler ikke bare om å lagre elementer. De legger grunnlaget for mer kompleks datamodellering ved å tillate operasjoner som søk, innsetting og fjerning i effektive tidskompleksiteter (logaritmisk for `std::map` og gjennomsnittlig konstant tid for `std::unordered_map`). Denne effektiviteten kommer fra de underliggende datastrukturene: et balansert tre for `std::map` og en hashtabell for `std::unordered_map`.

Historisk sett, før disse var en del av standardbiblioteket, måtte programmerere implementere sine egne versjoner eller bruke tredjeparts biblioteker, noe som førte til inkonsekvenser og potensielle ineffektiviteter. Inkluderingen av kart i C++'s standardbibliotek standardiserte ikke bare bruken av dem, men optimaliserte dem også for ytelse på tvers av ulike kompilatorer og plattformer.

Selv om begge er kraftige, avhenger valget mellom en `std::map` og `std::unordered_map` av spesifikasjonene i bruksområdet ditt. Trenger du ordnede data og bryr deg ikke om en liten ytelsesnedgang? Gå for `std::map`. Hvis du er etter hastighet og ikke bryr deg om rekkefølge, er `std::unordered_map` sannsynligvis et bedre valg.

Likevel er det viktig å merke seg at når man arbeider med komplekse datastrukturer, er det alltid avveininger. I noen nisjetilfeller kan andre datastrukturer eller til og med tredjepartsbiblioteker tilby bedre ytelse eller funksjonalitet egnet til dine spesielle behov. Vei alltid alternativene dine basert på kravene til prosjektet ditt.
