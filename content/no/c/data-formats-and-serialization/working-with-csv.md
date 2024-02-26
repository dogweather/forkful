---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:39.349340-07:00
description: "I programmeringsverdenen inneb\xE6rer arbeid med CSV-filer (kommaseparerte\
  \ verdier) \xE5 lese fra og skrive data til tekstfiler organisert i rader, der hver\
  \ rad\u2026"
lastmod: '2024-02-25T18:49:39.474087-07:00'
model: gpt-4-0125-preview
summary: "I programmeringsverdenen inneb\xE6rer arbeid med CSV-filer (kommaseparerte\
  \ verdier) \xE5 lese fra og skrive data til tekstfiler organisert i rader, der hver\
  \ rad\u2026"
title: Arbeide med CSV
---

{{< edit_this_page >}}

## Hva & Hvorfor?

I programmeringsverdenen innebærer arbeid med CSV-filer (kommaseparerte verdier) å lese fra og skrive data til tekstfiler organisert i rader, der hver rad representerer en post og hvert posts felt er adskilt med kommaer. Programmerere manipulerer CSV-filer for enkelhetens skyld ved dataimport/eksport på tvers av forskjellige systemer, på grunn av deres utbredte støtte og enkelhet for lagring av tabelldata.

## Hvordan:

### Lese CSV-filer
For å lese en CSV-fil i C, bruker vi standard fil-I/O-funksjoner sammen med strengmanipulasjonsfunksjoner for å analysere hver linje. Nedenfor er et grunnleggende eksempel på å lese en CSV-fil og skrive ut hvert felt i raden til konsollen.

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Kan ikke åpne filen\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *felt = strtok(buf, ",");
        while(felt) {
            printf("%s\n", felt);
            felt = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
Eksempel `data.csv`:
```
Navn,Alder,Yrke
Ola Nordmann,29,Programvareingeniør
```

Eksempelutskrift:
```
Navn
Alder
Yrke
Ola Nordmann
29
Programvareingeniør
```

### Skrive til CSV-filer
På samme måte innebærer skriving til en CSV-fil å bruke `fprintf` for å lagre data i et kommaseparert format.

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("output.csv", "w");
    if (!fp) {
        printf("Kan ikke åpne filen\n");
        return 1;
    }

    char *overskrifter[] = {"Navn", "Alder", "Yrke", NULL};
    for (int i = 0; overskrifter[i] != NULL; i++) {
        fprintf(fp, "%s%s", overskrifter[i], (overskrifter[i+1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Kari Nordmann", 27, "Dataanalytiker");

    fclose(fp);
    return 0;
}
```

Eksempel `output.csv` Innhold:
```
Navn,Alder,Yrke
Kari Nordmann,27,Dataanalytiker
```

## Dypdykk

CSV-formatet, selv om det tilsynelatende er enkelt, kommer med sine nyanser, som å håndtere kommaer innenfor felt og innkapsle felt med anførselstegn. De rudimentære eksemplene som er vist tar ikke hensyn til slike kompleksiteter, og de håndterer heller ikke potensielle feil robust.

Historisk sett har håndtering av CSV i C stort sett vært manuell på grunn av språkets lavnivå-natur og mangel på innebygde høynivå-abstraksjoner for slike oppgaver. Denne manuelle forvaltningen inkluderer å åpne filer, lese linjer, splitte strenger og konvertere datatyper etter behov.

Selv om direkte manipulering av CSV-filer i C gir verdifulle læringsopplevelser på fil-I/O og strengbehandling, lover flere moderne alternativer effektivitet og mindre feilutsatte prosesser. Biblioteker som `libcsv` og `csv-parser` tilbyr omfattende funksjoner for å lese og skrive CSV-filer, inkludert støtte for siterte felt og egendefinerte skilletegn.

Alternativt, når man arbeider innenfor økosystemer som støtter det, kan integrering med språk eller plattformer som tilbyr høynivå-CVS-manipulasjonsfunksjoner (som Python med sitt `pandas`-bibliotek) være en mer produktiv rute for applikasjoner som krever tung CSV-behandling. Denne tverrspråklige tilnærmingen utnytter Cs ytelse og systemprogrammeringskapasiteter samtidig som den bruker brukervennligheten fra andre språk for spesifikke oppgaver som CSV-håndtering.
