---
title:                "Loggning"
aliases:
- sv/cpp/logging.md
date:                  2024-01-26T01:01:11.381568-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggning"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/cpp/logging.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Loggning i programmeringssammanhang är processen att spela in händelser, tillstånd och information till en fil eller ett annat utgångsmedium. Programmerare loggar för att hålla koll på vad som händer i deras applikationer, för att felsöka problem, samt för att övervaka prestanda för framtida analys och optimering.

## Hur man gör:
Säg att du jobbar på en Linux-maskin och du vill slänga dina loggar i en fil med god gammal C++. Då vill du inkludera biblioteken `<iostream>` och `<fstream>` för att hantera filoperationer. Här är ett snabbt exempel:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream logFile("appLog.txt", std::ios::app);  // Öppna i läget för att lägga till

    if (!logFile.is_open()) {
        std::cerr << "Det uppstod ett problem med att öppna loggfilen!" << std::endl;
        return 1;
    }

    logFile << "Applikationen startad" << std::endl;
  
    // ... någonstans i din applikations logik
    logFile << "En viktig händelse har inträffat" << std::endl;

    // Glöm inte att stänga din filström
    logFile.close();

    return 0;
}
```

Om du följer din loggfil med `tail -f appLog.txt`, bör du se:

```
Applikationen startad
En viktig händelse har inträffat
```

Snyggt, du har nu en tidsstämplad händelselog!

## Djupdykning
Loggning är lika gammalt som datorberäkning i sig, med rötter i bokstavliga märken på papper för att spåra vad de forntida datorerna höll på med. I det moderna eran handlar allt om avancerade mjukvarulösningar. Du har rakt-på-fil-loggning, som det snabba och smutsiga exemplet ovan, eller så kanske du förlustar dig i ett finare loggningsramverk, såsom Log4cpp eller Boost.Log i C++ sfären; dessa tuffa killar erbjuder loggningsnivåer, formatkontroll och mer.

När det gäller nivåer inkluderar bästa praxis för loggning att använda varierande allvarlighetsnivåer—info, debug, varning, fel, fatal—så att du kan filtrera bort oväsentligheter när du försöker krossa buggar eller förstå varför din app beter sig som en lunefull tonåring.

På prestandanoteringen, bli inte slarvig med dina loggar. Överdriven loggning kan förvandla din blixtsnabba app till en snigelmaraton, tynga ner filsystem, eller till och med kosta dig pengar i lagringsavgifter om du är baserad i molnet. Att hitta rätt balans är nyckeln: logga det du behöver, och inget mer.

## Se även
För er som gillar att ta ert loggningsarbete ett steg längre, kolla in dessa:

- [Boost.Log-biblioteket](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html) för några tunga loggningsfunktioner.
- [Googles glog-bibliotek](https://github.com/google/glog) om du är intresserad av vad teknikjättens kockar använder för att logga deras appar.
- [Log4cpp-biblioteket](http://log4cpp.sourceforge.net/) för en konfigurerbar loggningsmekanism.

Och för lite bakgrundsläsning om varför och hur man loggar, djupdyk i:

- Denna Stack Overflow-tråd om [bästa praxis för loggning](https://stackoverflow.com/questions/783956/logging-best-practices) kommer att ge dig en djupgående granskning av ämnet från likasinnade.
