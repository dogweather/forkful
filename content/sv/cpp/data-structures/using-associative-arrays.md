---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:24.072921-07:00
description: "Associativa arrayer, k\xE4nda som `std::map` eller `std::unordered_map`\
  \ i C++, \xF6verbryggar klyftan mellan arrayindex och verkliga data, vilket l\xE5\
  ter dig\u2026"
lastmod: '2024-03-13T22:44:38.202066-06:00'
model: gpt-4-0125-preview
summary: "Associativa arrayer, k\xE4nda som `std::map` eller `std::unordered_map`\
  \ i C++, \xF6verbryggar klyftan mellan arrayindex och verkliga data, vilket l\xE5\
  ter dig anv\xE4nda meningsfulla nycklar."
title: "Att anv\xE4nda associativa arrayer"
weight: 15
---

## Vad & Varför?

Associativa arrayer, kända som `std::map` eller `std::unordered_map` i C++, överbryggar klyftan mellan arrayindex och verkliga data, vilket låter dig använda meningsfulla nycklar. De är det självklara valet när du behöver snabba uppslag, insättningar och raderingar med nycklar istället för indexpositioner.

## Hur man gör:

I C++ får associativa arrayer liv med `<map>` och `<unordered_map>`-biblioteken. Låt oss bryta ner exempel för att se båda i aktion.

### Använda `std::map`

`std::map` håller element sorterade baserade på nyckeln. Så här kommer du igång:

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ageMap;
    
    // Infoga värden
    ageMap["Alice"] = 30;
    ageMap["Bob"] = 25;
    
    // Komma åt värden
    std::cout << "Bobs ålder: " << ageMap["Bob"] << std::endl;
    
    // Iterera över en map
    for(const auto &pair : ageMap) {
        std::cout << pair.first << " är " << pair.second << " år gammal." << std::endl;
    }
    
    return 0;
}
```

### Använda `std::unordered_map`

När ordningen inte spelar någon roll, men prestanda gör det, är `std::unordered_map` din vän, som erbjuder snabbare genomsnittlig komplexitet för insättningar, uppslag och raderingar.

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> productPrice;
    
    // Infoga värden
    productPrice["mjölk"] = 2.99;
    productPrice["bröd"] = 1.99;
    
    // Komma åt värden
    std::cout << "Pris på mjölk: $" << productPrice["mjölk"] << std::endl;
    
    // Iterera över en unordered_map
    for(const auto &pair : productPrice) {
        std::cout << pair.first << " kostar $" << pair.second << std::endl;
    }
    
    return 0;
}
```

## Djupdykning

Associativa arrayer i C++, särskilt `std::map` och `std::unordered_map`, handlar inte bara om att lagra element. De ger grunden för mer komplex dataskötsel genom att tillåta operationer som sökning, infogning och borttagning i effektiva tidskomplexiteter (logaritmisk för `std::map` och genomsnittlig konstant tid för `std::unordered_map`). Denna effektivitet kommer från de underliggande datastrukturerna: ett balanserat träd för `std::map` och en hashtabell för `std::unordered_map`.

Historiskt sett, innan dessa var en del av standardbiblioteket, skulle programmerare behöva implementera sina egna versioner eller använda tredjepartsbibliotek, vilket ledde till inkonsekvenser och potentiella ineffektiviteter. Inkluderingen av maps i C++:s standardbibliotek standardiserade inte bara deras användning utan optimerade dem också för prestanda över olika kompilatorer och plattformar.

Även om båda är kraftfulla, beror valet mellan en `std::map` och en `std::unordered_map` på specifikationerna i ditt användningsfall. Behöver du ordnade data och inte bryr dig om en liten avvägning i prestanda? Gå med `std::map`. Om du är ute efter hastighet och inte bryr dig om ordning, är `std::unordered_map` troligen ditt bättre val.

Det är dock viktigt att notera att när man arbetar med komplexa datastrukturer finns det alltid avvägningar. I vissa nischfall kan andra datastrukturer eller till och med tredjepartsbibliotek erbjuda bättre prestanda eller funktionalitet som passar dina specifika behov. Väg alltid dina alternativ baserat på kraven i ditt projekt.
