---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:36.867731-07:00
description: "Kuinka: C++:ssa assosiatiiviset taulukot her\xE4\xE4v\xE4t eloon `<map>`\
  \ ja `<unordered_map>` otsikoiden kanssa. Katsotaan esimerkkej\xE4 n\xE4iden k\xE4\
  yt\xF6st\xE4. `std::map`\u2026"
lastmod: '2024-03-13T22:44:56.858070-06:00'
model: gpt-4-0125-preview
summary: "C++:ssa assosiatiiviset taulukot her\xE4\xE4v\xE4t eloon `<map>` ja `<unordered_map>`\
  \ otsikoiden kanssa."
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
weight: 15
---

## Kuinka:
C++:ssa assosiatiiviset taulukot heräävät eloon `<map>` ja `<unordered_map>` otsikoiden kanssa. Katsotaan esimerkkejä näiden käytöstä.

### Käyttäen `std::map`
`std::map` pitää elementit järjestettynä avaimen perusteella. Näin pääset alkuun:

```C++
#include <iostream>
#include <map>
#include <string>

int main() {
    std::map<std::string, int> ikaMap;
    
    // Arvojen lisääminen
    ikaMap["Alice"] = 30;
    ikaMap["Bob"] = 25;
    
    // Arvojen käyttö
    std::cout << "Bobin ikä: " << ikaMap["Bob"] << std::endl;
    
    // Iterointi mapin läpi 
    for(const auto &pari : ikaMap) {
        std::cout << pari.first << " on " << pari.second << " vuotta vanha." << std::endl;
    }
    
    return 0;
}
```

### Käyttäen `std::unordered_map`
Kun järjestys ei ole tärkeä, mutta suorituskyky on, `std::unordered_map` on ystäväsi, tarjoten nopeamman keskimääräisen monimutkaisuuden lisäyksille, hauille ja poistoille.

```C++
#include <iostream>
#include <unordered_map>
#include <string>

int main() {
    std::unordered_map<std::string, double> tuoteHinta;
    
    // Arvojen lisääminen
    tuoteHinta["maito"] = 2.99;
    tuoteHinta["leipä"] = 1.99;
    
    // Arvojen käyttö
    std::cout << "Maidon hinta: $" << tuoteHinta["maito"] << std::endl;
    
    // Iterointi unordered_mapin läpi
    for(const auto &pari : tuoteHinta) {
        std::cout << pari.first << " maksaa $" << pari.second << std::endl;
    }
    
    return 0;
}
```

## Syväsukellus
C++:n assosiatiiviset taulukot, erityisesti `std::map` ja `std::unordered_map`, eivät ole vain elementtien säilyttämistä varten. Ne tarjoavat perustan monimutkaisemmalle datan hallinnalle sallien operaatioita kuten haku, lisäys ja poisto tehokkaissa aikakompleksiteeteissa (logaritmisen `std::map`:lle ja keskimääräisen vakioajan `std::unordered_map`:lle). Tämä tehokkuus tulee niiden alla olevista tietorakenteista: tasapainotetusta puusta `std::map`:lle ja hajautustaulukosta `std::unordered_map`:lle.

Historiallisesti, ennen kuin nämä olivat osa standardikirjastoa, ohjelmoijien täytyi toteuttaa omia versioitaan tai käyttää kolmansien osapuolien kirjastoja, johtaen epäjohdonmukaisuuksiin ja potentiaalisiin tehokkuushäviöihin. Map:ien sisällyttäminen C++:n standardikirjastoon ei ainoastaan standardisoinut niiden käyttöä, vaan myös optimoi ne suorituskyvyltään eri kääntäjillä ja alustoilla.

Vaikka molemmat ovat tehokkaita, valinta `std::map` ja `std::unordered_map` välillä riippuu käyttötapaustesi erityiskohdista. Tarvitsetko järjestettyä dataa etkä välitä pienestä suorituskyvyn heikkenemisestä? Mene `std::map` kanssa. Jos olet nopeuden perässä etkä välitä järjestyksestä, `std::unordered_map` on todennäköisesti parempi valinta.

On kuitenkin tärkeää huomata, että työskennellessä monimutkaisten tietorakenteiden parissa, aina on kompromisseja. Joissakin niche-tapauksissa muut tietorakenteet tai jopa kolmansien osapuolien kirjastot saattavat tarjota parempaa suorituskykyä tai toiminnallisuutta sopien erityistarpeisiisi. Arvioi aina vaihtoehtojasi projektisi vaatimusten perusteella.
