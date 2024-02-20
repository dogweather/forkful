---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:58.657848-07:00
description: "Debuggere i C er spesialiserte verkt\xF8y som lar utviklere g\xE5 gjennom\
  \ koden sin trinn for trinn, inspisere variabler og f\xF8lge utf\xF8relsesflyten.\
  \ Denne\u2026"
lastmod: 2024-02-19 22:05:00.557922
model: gpt-4-0125-preview
summary: "Debuggere i C er spesialiserte verkt\xF8y som lar utviklere g\xE5 gjennom\
  \ koden sin trinn for trinn, inspisere variabler og f\xF8lge utf\xF8relsesflyten.\
  \ Denne\u2026"
title: "Bruke en feils\xF8ker"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Debuggere i C er spesialiserte verktøy som lar utviklere gå gjennom koden sin trinn for trinn, inspisere variabler og følge utførelsesflyten. Denne prosessen er integrert for å identifisere og fikse feil, og sikrer at koden oppfører seg som forventet.

## Hvordan:

GDB (GNU Debugger) er den mest brukte debuggeren for C-programmering. Her er en kort guide om hvordan du bruker GDB for å feilsøke et enkelt C-program.

Først, kompiler C-programmet ditt med `-g`-flagget for å inkludere feilsøkingsinformasjon:

```c
gcc -g program.c -o program
```

Deretter starter du GDB med det kompilerte programmet ditt:

```bash
gdb ./program
```

Du kan nå bruke ulike kommandoer innen GDB for å styre operasjonen. Her er noen grunnleggende kommandoer:

- `break`: Sett et brytningspunkt på en spesifisert linje eller funksjon for å pause utførelsen.
  - Eksempel: `break 10` eller `break main`
- `run`: Start utførelsen av programmet ditt innen GDB.
- `next`: Utfør neste linje med kode uten å stegge inn i funksjoner.
- `step`: Utfør neste linje med kode, stegger inn i funksjoner.
- `print`: Vis verdien av en variabel.
- `continue`: Gjenoppta utførelsen til neste brytningspunkt.
- `quit`: Avslutt GDB.

Her er et eksempel på en feilsøkingssesjon av et enkelt program:

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

Kompiler og start GDB som beskrevet. Sett et brytningspunkt ved `printf`-linjen med `break 5` og deretter `run`. Bruk `next` for å stegge gjennom løkken og `print i` for å inspisere løkkevariabelen.

Eksempel på utdata etter å ha satt et brytningspunkt og før første iterasjon:

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

Å bruke `print i` etter noen iterasjoner:

```
$3 = 2
```

Dette demonstrerer hvordan man undersøker tilstanden og flyten av et enkelt program.

## Dypdykk

Konseptet med feilsøking har utviklet seg betydelig siden de tidlige dagene av programmering, der fysiske feil (bokstavelige insekter) kunne forårsake problemer i mekaniske datamaskiner. I dag tilbyr debuggere som GDB sofistikerte funksjoner utover grunnleggende stepping og variabelinspeksjon, som omvendt feilsøking (utføre programmet baklengs), betingede brytningspunkter, og skripting for automatiserte feilsøkingsoppgaver.

Selv om GDB er kraftfullt og mye brukt, kan det være tett og utfordrende for nybegynnere. Alternative feilsøkingsverktøy og IDE-er (Integrated Development Environments) som Visual Studio Code, CLion, eller Eclipse tilbyr mer brukervennlige grensesnitt for feilsøking av C-kode, ofte med visuelle hjelpemidler og mer intuitive kontroller. Disse alternativene tilbyr kanskje ikke hele dybden av funksjonalitet som GDB gjør, men kan være mer tilgjengelig for nykommere til C-programmering.

Videre har fremveksten av språkserverprotokoller og feilsøkingsstandarder tilrettelagt for plattformuavhengige feilsøkingsløsninger, noe som gjør feilsøkingsopplevelsen mer konsistent på tvers av ulike verktøy og miljøer. Til tross for disse fremskrittene, er det å lære inngående om en tradisjonell debugger som GDB uvurderlig innsikt i utførelsen av C-programmer og forblir en avgjørende ferdighet i en utviklers verktøykasse.
