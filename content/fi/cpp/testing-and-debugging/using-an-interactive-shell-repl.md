---
date: 2024-01-26 04:12:13.102310-07:00
description: "Kuinka: C++ ei sis\xE4ll\xE4 sis\xE4\xE4nrakennettua REPL:i\xE4, mutta\
  \ ty\xF6kalut, kuten Cling, tarjoavat kyseisen mahdollisuuden. T\xE4ss\xE4 on ohjeet,\
  \ kuinka k\xE4ytt\xE4\xE4 Clingi\xE4\u2026"
lastmod: '2024-03-13T22:44:56.866588-06:00'
model: gpt-4-0125-preview
summary: "C++ ei sis\xE4ll\xE4 sis\xE4\xE4nrakennettua REPL:i\xE4, mutta ty\xF6kalut,\
  \ kuten Cling, tarjoavat kyseisen mahdollisuuden."
title: "Interaktiivisen komentotulkin (REPL) k\xE4ytt\xF6"
weight: 34
---

## Kuinka:
C++ ei sisällä sisäänrakennettua REPL:iä, mutta työkalut, kuten Cling, tarjoavat kyseisen mahdollisuuden. Tässä on ohjeet, kuinka käyttää Clingiä kahden numeron summan laskemiseen:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "Summa on: " << a + b << std::endl;
    return 0;
}

// Tuloste:
// Summa on: 12
```

Käynnistä Cling ja syötä koodi rivi riviltä, tarkkaillen tulostetta jokaisen komennon jälkeen. Se on välitöntä palautetta, ilman kääntämistä.

## Syväsukellus
REPL:iä on yleinen erityisesti kielille, kuten Python tai Lisp, ja ne ovat olleet olemassa 1960-luvulta lähtien. C++:lle, käännettävälle kielelle, konsepti ei sovi yhtä luonnollisesti, minkä vuoksi työkalut kuten Cling ovat olemassa - ne tulkitsevat C++:aa lennosta. Vaihtoehtoja sisältävät verkkokääntäjät tai pienet koeohjelmat, jotka on perinteisesti käännetty. Cling on rakennettu LLVM:n ja Clangin päälle, tarjoten sillan C++:n käytölle tulkattuna muodossa.

## Katso Myös
- [Cling](https://root.cern/cling/): Interaktiivinen C++ tulkki, rakennettu LLVM:n ja Clang-kirjastojen päällä.
- [Jupyter Notebooks](https://jupyter.org/): Tarjoaa vuorovaikutteisen kuoren muistikirjaympäristössä, tukee C++:aa xeus-cling-ytimen kautta.
- [LLVM](https://llvm.org/): Kokoelma modulaarisia ja uudelleenkäytettäviä kääntäjä- ja työkaluketjutekniikoita, joiden päälle Cling on rakennettu.
