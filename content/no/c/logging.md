---
title:                "Loggføring"
date:                  2024-01-26T01:00:31.132425-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggføring"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/logging.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Logging er i hovedsak det å notere ned hva programmet ditt gjør, typisk ved å skrive ut meldinger til en fil eller terminal. Programmerere gjør dette for å holde oversikt over hendelser, diagnostisere problemer, og for å ha en revisjonsspor som forteller historien om en applikasjons drift over tid.

## Hvordan:
La oss starte med noen grunnleggende ferdigheter. C har ikke et innebygd rammeverk for logging, men du kan rulle sammen noe enkelt med `stdio.h`. Her er hvordan:

```c
#include <stdio.h>
#include <time.h>

void logMessage(const char* message) {
    time_t now;
    time(&now);
    char *dato = ctime(&now);
    dato[strlen(dato) - 1] = '\0'; // Fjerner newline på slutten av ctime()'s resultat
    printf("[%s] %s\n", dato, message);
}

int main() {
    logMessage("Applikasjonen har startet.");
    // ... din kode går her ...
    logMessage("Applikasjonen gjør noe viktig.");
    // ... din kode fortsetter ...
    logMessage("Applikasjonen har avsluttet.");
    return 0;
}
```

Eksempel på output kan se slik ut:

```
[Tir Mar 9 12:00:01 2023] Applikasjonen har startet.
[Tir Mar 9 12:00:02 2023] Applikasjonen gjør noe viktig.
[Tir Mar 9 12:00:03 2023] Applikasjonen har avsluttet.
```

Selvfølgelig, i den virkelige verden ville man sannsynligvis ønske å skrive til en fil i stedet for terminalen, håndtere forskjellige loggnivåer, og kanskje bruke et forhåndsdefinert bibliotek.

## Dypdykk
Logging i C har en sjarmerende gammeldags følelse – det er like lavnivå som det meste av resten av språket. Historisk sett ble logging utført ved å bruke `fprintf` med `stderr` eller en filpeker. Ettersom programmer ble mer komplekse, ble også behovene for logging det, noe som førte til utviklingen av biblioteker som `syslog` på Unix-systemer, som kunne håndtere logging fra flere kilder med ulike viktighetsnivåer.

I det moderne landskapet finnes det mange C logging biblioteker der ute, som `zlog`, `log4c`, og `glog`, som tilbyr et rikt utvalg av funksjoner inkludert loggrotasjon, strukturert logging og flertrådet logging. Disse løsningene tillater finjustert kontroll over logg verbositet, destinasjoner og formater.

Når man implementerer et loggsystem, trenger detaljer som tidsstempelformatering, loggfilhåndtering og ytelse vurdering. Tidsstempling av logger er avgjørende for å korrelere hendelser, mens loggrotasjon sikrer at loggfiler ikke bruker for mye diskplass. Handlingen med å logge bør også være rask og ikke-blokkerende for hovedapplikasjonsflyten for å forhindre at logging blir en flaskehals.

## Se også
For å dykke dypere inn i logging biblioteker og praksiser i C, sjekk ut disse ressursene:

- GNU `syslog` manual: https://www.gnu.org/software/libc/manual/html_node/Syslog.html
- `zlog`: Et svært konfigurerbart logging bibliotek for C - https://github.com/HardySimpson/zlog
- `log4c`: Et logging rammeverk for C modellert etter Log4j - http://log4c.sourceforge.net/
- `glog`: Googles applikasjonsnivå logging bibliotek - https://github.com/google/glog
