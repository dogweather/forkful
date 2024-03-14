---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:55:20.133674-07:00
description: "\xC5 opprette en midlertidig fil i C inneb\xE6rer \xE5 generere en fil\
  \ som er ment \xE5 bli brukt over en kort varighet, vanligvis som rableplass for\
  \ databehandling\u2026"
lastmod: '2024-03-13T22:44:41.292868-06:00'
model: gpt-4-0125-preview
summary: "\xC5 opprette en midlertidig fil i C inneb\xE6rer \xE5 generere en fil som\
  \ er ment \xE5 bli brukt over en kort varighet, vanligvis som rableplass for databehandling\u2026"
title: Oppretting av en midlertidig fil
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å opprette en midlertidig fil i C innebærer å generere en fil som er ment å bli brukt over en kort varighet, vanligvis som rableplass for databehandling eller lagring. Programmerere gjør dette for å håndtere midlertidige data uten å påvirke programmets permanente lagring eller for å sikre at sensitiv informasjon slettes etter bruk.

## Hvordan:
Å skape en midlertidig fil i programmeringsspråket C kan dra nytte av funksjoner som `tmpfile()` og `mkstemp()`.

**Bruke `tmpfile()`**: Denne funksjonen skaper en unik midlertidig fil som automatisk slettes når programmet avsluttes eller filen lukkes.

```c
#include <stdio.h>

int main() {
    FILE *temp = tmpfile();
    hvis (temp == NULL) {
        perror("Klarte ikke å opprette midlertidig fil");
        return 1;
    }

    // Skrive data til den midlertidige filen
    fputs("Dette er en test.\n", temp);

    // Spole tilbake og lese det vi skrev
    rewind(temp);
    char buffer[1024];
    while (fgets(buffer, sizeof(buffer), temp) != NULL) {
        printf("%s", buffer);
    }

    // Automatisk slettet ved lukking eller programavslutning
    fclose(temp);

    return 0;
}
```
**Eksempel på utdata:**
```
Dette er en test.
```

**Bruke `mkstemp()`**: Gir mer kontroll over den midlertidige filens plassering og tillatelser. Den krever en malstreng som ender med `XXXXXX` som den deretter erstatter med en unik sekvens for å forhindre navnesammenstøt.

```c
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>

int main() {
    char mal[] = "/tmp/mytemp-XXXXXX";
    int fd = mkstemp(mal);

    hvis (fd == -1) {
        perror("Klarte ikke å opprette midlertidig fil");
        return 1;
    }
    
    printf("Midlertidig fil opprettet: %s\n", mal);

    // Midlertidige filer opprettet med mkstemp() bør slettes manuelt
    unlink(mal);

    close(fd);
    return 0;
}
```
**Eksempel på utdata:**
```
Midlertidig fil opprettet: /tmp/mytemp-abc123
```

## Dypdykk
Konseptet med midlertidige filer er ikke unikt for C, men er en felles funksjonalitet i mange programmeringsmiljøer på grunn av den praktiske bruken i håndtering av flyktige data. `tmpfile()`-funksjonen, standardisert i ISO C-standarden, skaper en fil med et unikt navn i et standard katalog, men dens eksistens er flyktig, noe som gjør den ideell for sikre eller midlertidige operasjoner.

En bemerkelsesverdig begrensning av `tmpfile()` er dens avhengighet av standard midlertidig katalog, som kanskje ikke er passende for alle applikasjoner spesielt med tanke på tillatelser eller sikkerhet. I motsetning gir `mkstemp()` muligheten til å spesifisere katalogen og sikrer sikker filoppretting med garantert unike filnavn ved å endre den angitte malstrengen, og tilbyr en mer allsidig løsning på bekostning av manuell filbehandling.

Opprettelse av midlertidige filer kan imidlertid introdusere sikkerhetsrisikoer, som race conditions, hvis de ikke håndteres riktig. For eksempel, `tmpfile()` og `mkstemp()` adresserer ulike aspekter av sikker midlertidig filoppretting (automatisk sletting og sikker navngenerering, henholdsvis), men ingen av dem er en universalløsning. Utviklere må vurdere spesifikkene til deres applikasjons sikkerhetsbehov, inkludert potensielle sårbarheter introdusert av midlertidige filer, og kan trenge å implementere ekstra sikkerhetstiltak utover det disse funksjonene tilbyr.

I det bredere programmeringslandskapet kan alternativer som lagring i minnet (f.eks., ved å bruke dynamiske datastrukturer eller minnekartlagte filer) tilby bedre ytelse eller sikkerhet for håndtering av midlertidige data. Likevel, fysiske midlertidige filer forblir et avgjørende verktøy i mange scenarioer, spesielt for store datasett eller når inter-prosesskommunikasjon er involvert.
