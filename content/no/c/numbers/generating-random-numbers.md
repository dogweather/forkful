---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:29.467098-07:00
description: "\xC5 generere tilfeldige tall i C inneb\xE6rer \xE5 skape verdier som\
  \ er uforutsigbare og f\xF8lger en spesifikk fordeling, som uniform eller normal.\
  \ Denne evnen er\u2026"
lastmod: '2024-03-13T22:44:41.267159-06:00'
model: gpt-4-0125-preview
summary: "\xC5 generere tilfeldige tall i C inneb\xE6rer \xE5 skape verdier som er\
  \ uforutsigbare og f\xF8lger en spesifikk fordeling, som uniform eller normal. Denne\
  \ evnen er\u2026"
title: Genererer tilfeldige tall
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall i C innebærer å skape verdier som er uforutsigbare og følger en spesifikk fordeling, som uniform eller normal. Denne evnen er avgjørende for applikasjoner som strekker seg fra simuleringer og spill til kryptografiske operasjoner, der uforutsigbarhet eller simulering av virkelighetens tilfeldighet er essensiell.

## Hvordan:

I C kan tilfeldige tall genereres ved å bruke `rand()`-funksjonen, som er en del av C-standardbiblioteket `<stdlib.h>`. Som standard produserer `rand()` pseudo-tilfeldige tall i området fra 0 til `RAND_MAX` (en konstant definert i `<stdlib.h>`). For mer kontroll over området, kan programmerere manipulere utdata fra `rand()`.

Her er et enkelt eksempel på å generere et tilfeldig tall mellom 0 og 99:

```c
#include <stdio.h>
#include <stdlib.h> // For rand() og srand()
#include <time.h>   // For time()

int main() {
    // Så frø for tilfeldighetstallsgeneratoren
    srand((unsigned) time(NULL));

    // Generer et tilfeldig tall mellom 0 og 99
    int randomNumber = rand() % 100;

    printf("Tilfeldig Tall: %d\n", randomNumber);

    return 0;
}
```

Eksempel på utdata kan variere hver gang du kjører dette programmet:

```
Tilfeldig Tall: 42
```
For å generere tilfeldige tall innenfor et annet område, kan du justere modulusoperatoren (`%`) deretter. For eksempel vil `rand() % 10` generere tall fra 0 til 9.

Det er viktig å merke seg at det å så frø for pseudo-tilfeldighetsgeneratoren (`srand()`-kallet) med nåværende tid (`time(NULL)`) sikrer forskjellige sekvenser av tilfeldige tall på tvers av programutførelser. Uten såing (`srand()`), ville `rand()` produsere den samme sekvensen av tall hver gang programmet blir kjørt.

## Dypdykk

`rand()`-funksjonen og dens partner for såing `srand()` har vært en del av C-standardbiblioteket i flere tiår. De er basert på algoritmer som genererer sekvenser av tall som bare ser ut til å være tilfeldige – derav betegnelsen "pseudo-tilfeldig". Den underliggende algoritmen i `rand()` er vanligvis en lineær kongruent generator (LCG).

Selv om `rand()` og `srand()` er tilstrekkelige for mange applikasjoner, har de kjente begrensninger, spesielt angående kvaliteten på tilfeldigheten og potensiell forutsigbarhet. For applikasjoner som krever høykvalitativ tilfeldighet, som kryptografiske operasjoner, bør alternativer som `/dev/random` eller `/dev/urandom` (på Unix-lignende systemer), eller API-er levert av kryptografiske biblioteker, vurderes.

Med introduksjonen av C11 inkluderte ISO C-standarden en ny header, `<stdatomic.h>`, som tilbyr mer raffinert kontroll for samtidige operasjoner, men ikke direkte angående tilfeldighet. For ekte tilfeldighet i C, vender utviklere seg ofte til plattformspesifikke eller eksterne biblioteker som tilbyr bedre algoritmer eller benytter seg av maskinvareentropikilder.

Husk, mens `rand()` fungerer som et enkelt og tilgjengelig middel for å generere pseudo-tilfeldige tall, er dets bruksområder i moderne applikasjoner begrenset av kvaliteten og forutsigbarheten av dens utdata. Når mer robuste løsninger er nødvendige, spesielt for sikkerhetsbevisste applikasjoner, er det sterkt anbefalt å utforske utover standardbiblioteket.
