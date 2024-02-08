---
title:                "Debuggerin käyttö"
aliases:
- fi/cpp/using-a-debugger.md
date:                  2024-01-26T03:48:00.348649-07:00
model:                 gpt-4-0125-preview
simple_title:         "Debuggerin käyttö"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/cpp/using-a-debugger.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Debuggerin käyttäminen tarkoittaa työkalun käynnistämistä, joka antaa sinun kurkistaa käynnissä olevan ohjelmasi sisään ymmärtääksesi, mitä todella tapahtuu. Ohjelmoijat tekevät tämän löytääkseen ja nujertaakseen bugeja—niitä ärsyttäviä ongelmia, jotka saavat koodisi käyttäytymään odottamattomasti tai kaatumaan.

## Kuinka:
C++ integroituu debuggerien, kuten GDB:n tai Visual Studio debuggerin kanssa. Tässä on pieni esimerkki GDB:n käytöstä:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    int c = a / b; // Hups, jakaminen nollalla!
    std::cout << c << std::endl;
    return 0;
}

// Käännä käyttäen:
// g++ -g -o my_program my_program.cpp

// Aja debuggerin kanssa:
// gdb ./my_program
```

Kun olet käynnistänyt GDB:n, voit asettaa keskeytyskohtia, kulkea koodisi läpi askel askeleelta, tarkastella muuttujia ja paljon muuta. Jos suoritat yllä olevan, sinun pitäisi nähdä ohjelmasi kaatuvan nollalla jakamisen vuoksi.

## Syväsukellus
Debuggaaminen juontaa juurensa ohjelmoinnin alkuaikoihin, jolloin oli tarpeen kirjaimellisesti poistaa bugeja (hyönteisiä!) laitteistosta. Siitä lähtien debuggaustyökalut ovat kehittyneet monimutkaisiksi ja tehokkaiksi ohjelmistoiksi, jotka ovat kriittisiä kehityksen kannalta.

Vaihtoehtoja GDB:lle C++:lla sisältävät LLDB:n sekä IDE:ihin integroidut debuggerit kuten ne Visual Studio:ssa, CLion:ssa tai Eclipse:ssä. Nämä nykyaikaiset ympäristöt tarjoavat graafisia käyttöliittymiä, jotka tekevät debuggauksesta vähemmän pelottavaa.

Debuggerin käyttöön liittyvät toteutusyksityiskohdat riippuvat usein kehitysympäristöstäsi:

- Komentorividebuggerit (GDB, LLDB) vaativat tuttuutta terminaalikomentojen kanssa ja usein niillä on jyrkempi oppimiskäyrä.
- Graafiset debuggerit yksinkertaistavat prosessia sallimalla piste-ja-klikkaa-interaktiot asettaaksesi keskeytyskohtia, kulkea läpi koodia ja tarkkailla muuttujia.

Ymmärtäminen debuggerisi kyvykkyyksistä, kuten ehdolliset keskeytyskohdat, tarkkailupisteet tai lausekkeiden arviointi, voi merkittävästi lisätä tehokkuuttasi ongelmien diagnosoinnissa.

## Katso Myös
- [GDB Dokumentaatio](https://www.gnu.org/software/gdb/documentation/)
- [LLDB Komentodokumentaatio](https://lldb.llvm.org/use/map.html)
- [Visual Studio Debugger Tutustumisopas](https://docs.microsoft.com/en-us/visualstudio/debugger/debugger-feature-tour)
- [Debuggaus CLionilla](https://www.jetbrains.com/help/clion/debugging-code.html)
