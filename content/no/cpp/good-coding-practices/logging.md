---
title:                "Loggføring"
aliases: - /no/cpp/logging.md
date:                  2024-01-26T01:01:07.800393-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggføring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/logging.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Logging i programmeringssammenheng er prosessen med å registrere hendelser, tilstander og informasjon til en fil eller annet utgangsmedium. Programmerere logger for å holde oversikt over hva som skjer i applikasjonene sine, for å feilsøke problemer, og for å overvåke ytelse til fremtidig analyse og optimalisering.

## Hvordan:
La oss si at du jobber på en Linux-maskin og du vil kaste loggene dine til en fil med god gammeldags C++. Du vil inkludere bibliotekene `<iostream>` og `<fstream>` for å håndtere filoperasjoner. Her er et raskt eksempel:

```C++
#include <iostream>
#include <fstream>
#include <string>

int main() {
    std::ofstream logFile("appLog.txt", std::ios::app);  // Åpne i påføringsmodus

    if (!logFile.is_open()) {
        std::cerr << "Det var et problem med å åpne loggfilen!" << std::endl;
        return 1;
    }

    logFile << "Applikasjonen startet" << std::endl;
  
    // ... et sted i applogikken din
    logFile << "En viktig hendelse har inntruffet" << std::endl;

    // Glem ikke å lukke filstrømmen din
    logFile.close();

    return 0;
}
```

Hvis du følger loggfilen din med `tail -f appLog.txt`, burde du se:

```
Applikasjonen startet
En viktig hendelse har inntruffet
```

Pent, du har nå en tidsstemplet registrering av hendelser!

## Dypdykk
Logging er like gammel som databehandling selv, med røtter i bokstavelige merker på papir for å spore hva de antikke datamaskinene holdt på med. I den moderne æraen handler det om sofistikerte programvareløsninger. Du har rett-til-fil-logging, som det kjappe og enkle eksempelet over, eller du kan kanskje fråtse i et mer fancy loggingrammeverk, som Log4cpp eller Boost.Log i C++-verdenen; disse tøffingene tilbyr loggnivåer, formatkontroll og mer.

Når vi snakker om nivåer, inkluderer beste praksis for logging å bruke varierende nivåer av alvorlighetsgrad – info, debug, advarsel, error, fatal – slik at du kan filtrere bort støy når du prøver å knuse bugs eller finne ut hvorfor appen din oppfører seg som en humørsyk tenåring.

Når det gjelder ytelse, ikke bli slurvete med loggene dine. Overdreven logging kan gjøre din lynraske app til en sneglemaraton, tynge ned filsystemer, eller til og med koste deg penger i lagringsgebyrer hvis du er basert i skyen. Å finne riktig balanse er nøkkelen: logg hva du trenger, og ikke mer.

## Se Også
For dere som liker å gå ekstra langt med loggingspraksisen deres, sjekk ut disse:

- [Boost.Log-biblioteket](https://www.boost.org/doc/libs/1_75_0/libs/log/doc/html/index.html) for noen kraftige loggingsfunksjoner.
- [Googles glog-bibliotek](https://github.com/google/glog) hvis du er interessert i hva teknologigigantens kokker bruker for å logge appene sine.
- [Log4cpp-biblioteket](http://log4cpp.sourceforge.net/) for en konfigurerbar loggingsmekanisme.

Og for litt bakgrunnslesning om hvorfor og hvordan man logger, dykk inn i:

- Denne Stack Overflow-tråden om [beste praksis for logging](https://stackoverflow.com/questions/783956/logging-best-practices) vil gi deg et fagfellevurdert dypdykk i emnet.
