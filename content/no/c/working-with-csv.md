---
title:                "C: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-csv.md"
---

{{< edit_this_page >}}

#Hvorfor

CSV-filer er et vanlig format for å lagre og dele data i mange bransjer. Som C-programmerer kan du dra nytte av å lære hvordan du kan lese og behandle denne typen filer. Dette kan hjelpe deg med å effektivt importere og analysere store datasett i dine programmer, noe som kan gi en stor fordel for deg og ditt team.

#Slik gjør du det

For å begynne å arbeide med CSV-filer i C, må du først inkludere <stdio.h> og <string.h> bibliotekene i koden din. Dette vil gi deg tilgang til funksjoner som du kan bruke til å lese og manipulere data i CSV-filer.

```C
#include <stdio.h>
#include <string.h>
```

For å lese en CSV-fil, må du først åpne den ved hjelp av fopen() funksjonen og spesifisere "r" modus for å lese filen. Deretter kan du bruke fgets() funksjonen til å lese en linje om gangen og strtok() funksjonen til å tokenize den uleste linjen basert på komma-separasjonen.

```C
FILE* file = fopen("exempelfil.csv", "r");
char line[256];
while(fgets(line, 256, file)){
  char *token = strtok(line, ",");
  while(token != NULL){
    // Gjør noe med dataen
    token = strtok(NULL, ",");
  }
}
```

Etter å ha behandlet dataen, må du huske å lukke filen ved å bruke fclose() funksjonen. For å skrive til en ny CSV-fil, kan du følge en lignende fremgangsmåte og bruke fprintf() funksjonen til å skrive data til filen.

```C
FILE* ny_fil = fopen("ny_exempelfil.csv", "w");
while(noe_data){
  fprintf(ny_fil, "%.2f,%.2f,%.2f", tall1, tall2, tall3);
}
fclose(ny_fil);
```

#Dypdykk

Når du jobber med CSV-filer i C, er det viktig å være oppmerksom på eventuelle problemer som kan oppstå. For eksempel kan data i celler være adskilt av forskjellige skilletegn, som komma, semikolon eller tabulatorer. Dette kan føre til feil i lesingen av dataen hvis du ikke håndterer det riktig.

Det er også viktig å håndtere spesielle tegn og linjeskift inne i dataen. For å unngå dette kan du bruke en linjeskiftfunksjon som fgets() for å lese en linje om gangen og unngå problemer med å lese flere linjer på en gang.

Til slutt er det viktig å sikre at du har riktig formatering når du skriver til en CSV-fil. Det kan være lurt å sjekke om dataene dine er i tråd med standard CSV-format og eventuelt bruke funksjoner som fprintf() til å håndtere spesielle tegn og eventuelt sikre at dataene er adskilt av kommaer.

#Se også

- [Les og skriv CSV-filer i C](https://www.programiz.com/c-programming/c-file-reading-writing)
- [Eksempler på lesing og skriving av CSV-filer i C](https://www.codesdope.com/blog/article/reading-a-csv-file-in-c/)
- [Håndtere spesielle tegn i CSV-filer i C](https://stackoverflow.com/questions/11249291/how-to-handle-special-characters-in-csv-files)