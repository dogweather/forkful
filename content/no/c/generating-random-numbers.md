---
title:                "Generering av tilfeldige tall"
date:                  2024-01-27T20:33:15.643041-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generering av tilfeldige tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall i C innebærer å skape sekvenser av tall som mangler et gjenkjennelig mønster, for å etterligne konseptet med tilfeldighet. Programmerere utnytter tilfeldige tall til en rekke formål, inkludert simulering av data, kryptografiske applikasjoner og spillutvikling, noe som gjør det til et viktig aspekt ved programmering.

## Hvordan:

For å generere tilfeldige tall i C, bruker du vanligvis `rand()`-funksjonen som finnes i `stdlib.h`. Det er imidlertid avgjørende å initiere generatoren for tilfeldige tall for å sikre variabilitet i de genererte tallene over forskjellige programkjøringer. `srand()`-funksjonen, initiert med en verdi, ofte gjeldende tid, legger til rette for dette.

Her er et enkelt eksempel på å generere et tilfeldig tall mellom 0 og 99:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Initialiser generatoren for tilfeldige tall
    srand((unsigned) time(NULL));

    // Generer et tilfeldig tall mellom 0 og 99
    int randomNumber = rand() % 100;

    // Skriv ut det tilfeldige tallet
    printf("Tilfeldig tall: %d\n", randomNumber);

    return 0;
}
```

Eksempel på utskrift:

```
Tilfeldig tall: 42
```

Det er viktig å merke seg at hver utførelse av dette programmet vil produsere et nytt tilfeldig tall, takket være seeding med nåværende tid.

## Dypdykk

Den tradisjonelle måten å generere tilfeldige tall på i C, ved bruk av `rand()` og `srand()`, er ikke virkelig tilfeldig. Den er pseudotilfeldig. Dette er greit for mange applikasjoner, men det kommer til kort i situasjoner som krever høy grad av tilfeldighet, slik som i seriøse kryptografiske bruksområder. Sekvensen generert av `rand()` er helt bestemt av frøet gitt til `srand()`. Dermed, hvis frøet er kjent, kan sekvensen forutsies, noe som reduserer tilfeldigheten.

Historisk sett har `rand()`-funksjonen blitt kritisert for sin lave kvalitet på tilfeldighet og begrensede rekkevidde. Moderne alternativer inkluderer bruk av enhetsspesifikke API-er eller eksterne biblioteker som bedre nærmer seg ekte tilfeldighet, eller, i UNIX-lignende systemer, lese fra `/dev/random` eller `/dev/urandom` for kryptografiske formål.

For eksempel, ved bruk av `/dev/urandom` i C:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // Åpne /dev/urandom for lesing
    fp = fopen("/dev/urandom", "r");

    // Les et tilfeldig tall
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // Skriv ut det tilfeldige tallet
    printf("Tilfeldig tall: %u\n", randomNumber);

    // Lukk filen
    fclose(fp);

    return 0;
}
```

Denne metoden leser direkte fra systemets entropipool og tilbyr en høyere kvalitet på tilfeldighet som passer til mer følsomme applikasjoner. Imidlertid kan denne fremgangsmåten ha bærbarhetsproblemer på tvers av forskjellige plattformer, noe som gjør den mindre universell enn å bruke `rand()`.

Uavhengig av metoden, er det avgjørende å forstå naturen til tilfeldighet og dens implementering i C for å utvikle effektive, sikre og engasjerende applikasjoner.
