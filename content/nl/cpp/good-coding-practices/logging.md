---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:38.735984-07:00
description: "Loggen in de context van programmeren is het proces van het vastleggen\
  \ van gebeurtenissen, toestanden en informatie in een bestand of een ander\u2026"
lastmod: '2024-03-13T22:44:51.119385-06:00'
model: gpt-4-0125-preview
summary: Loggen in de context van programmeren is het proces van het vastleggen van
  gebeurtenissen, toestanden en informatie in een bestand of een ander uitvoermedium.
title: Logboekregistratie
weight: 17
---

## Hoe:
Stel je voor dat je aan een Linux box werkt en je wilt je logs in een bestand gooien met goed 'ol C++. Je wilt de bibliotheken `<iostream>` en `<fstream>` includeren om bestandsoperaties te doen. Hier is een snel voorbeeld:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream logBestand("appLog.txt", std::ios::app);  // Openen in append-modus

    if (!logBestand.is_open()) {
        std::cerr << "Er was een probleem met het openen van het logbestand!" << std::endl;
        return 1;
    }

    logBestand << "Applicatie gestart" << std::endl;
  
    // ... ergens in je app-logica
    logBestand << "Een belangrijke gebeurtenis heeft plaatsgevonden" << std::endl;

    // Vergeet niet je bestandsstream te sluiten
    logBestand.close();

    return 0;
}
```

Als je je logbestand volgt met `tail -f appLog.txt`, zou je moeten zien:

```
Applicatie gestart
Een belangrijke gebeurtenis heeft plaatsgevonden
```

Netjes, je hebt een getijdstempeld record van gebeurtenissen!

## Diepgaand
Loggen is zo oud als de computer zelf, met wortels in letterlijke markeringen op papier om te traceren wat ancient computers deden. In het moderne tijdperk gaat het allemaal om geavanceerde softwareoplossingen. Je hebt direct-naar-bestand loggen, zoals het snelle en vuile voorbeeld hierboven, of je kunt genieten van een chiquer logframework, zoals Log4cpp of Boost.Log in het C++ domein; deze jongens bieden logniveaus, controle over het formaat, en meer.

Over niveaus gesproken, best practices bij loggen omvatten het gebruik van verschillende niveaus van ernst—info, debug, waarschuwing, fout, fataal—zodat je het kaf van het koren kunt scheiden wanneer je probeert bugs te verpletteren of uit te zoeken waarom je app zich gedraagt als een humeurige tiener.

Op het gebied van prestaties, word niet slordig met je logs. Overmatig loggen kan je bliksemsnelle app veranderen in een slakkenmarathon, bestandssystemen vertragen, of zelfs kosten in opslaggelden als je cloudgebaseerd bent. Het juiste evenwicht vinden is de sleutel: log wat je nodig hebt, en niets meer.

## Zie Ook
Voor degenen onder jullie die een extra stap willen zetten met jullie logpraktijken, bekijk deze:

- De [Boost.Log bibliotheek](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html) voor wat zware logfuncties.
- [Google's glog bibliotheek](https://github.com/google/glog) als je geïnteresseerd bent in wat de tech-gigant's koks gebruiken om hun apps te loggen.
- [De Log4cpp bibliotheek](http://log4cpp.sourceforge.net/) voor een configureerbaar logmechanisme.

En voor wat achtergrondlezing over de waaroms en hoe van loggen, duik in:

- Deze Stack Overflow thread over [best practices bij loggen](https://stackoverflow.com/questions/783956/logging-best-practices) geeft je een door vakgenoten beoordeelde diepgaande duik in het onderwerp.
