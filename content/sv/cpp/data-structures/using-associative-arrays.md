---
title:                "Att använda associativa arrayer"
aliases:
- sv/cpp/using-associative-arrays.md
date:                  2024-01-30T19:10:24.072921-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda associativa arrayer"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
