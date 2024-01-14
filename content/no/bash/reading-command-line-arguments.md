---
title:                "Bash: Lesing av kommandolinje-argumenter"
programming_language: "Bash"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du noen gang har brukt Terminal eller kommandolinje på din datamaskin, har du sannsynligvis sett at programmer og verktøy kan ta argumenter eller parametere. Men hva er egentlig disse argumentene og hvorfor er det viktig å kunne lese dem? Les videre for å finne ut mer!

# Hvordan

Når du kjører et Bash-program, kan du legge til argumenter etter kommandoene. Disse argumentene kan være enkeltord eller setninger og de representerer informasjon som programmet vil bruke til å utføre en spesifikk oppgave. For å lese disse argumentene i programmet ditt, må du bruke variabler som heter $1, $2, $3, og så videre. Disse variablene representerer posisjonen til argumentene dine, slik at du enkelt kan få tilgang til dem.

La oss se på et enkelt eksempel. Vi vil lage et Bash-program som tar inn to tall og skriver ut summen av dem.

```Bash
echo "Summen av tallene er: $1 + $2 = $(($1 + $2))"
```

Her bruker vi variablene $1 og $2 til å få tak i de to tallene som brukeren skriver inn som argumenter. Vi bruker deretter disse tallene til å beregne summen og skrive den ut på skjermen. La oss si at vi kaller programmet vårt "sum.sh" og kjører det med argumentene "5" og "7". Da vil output være:

```
Summen av tallene er: 5 + 7 = 12
```

Veldig enkelt, ikke sant? Dette er bare et enkelt eksempel, men det viser hvordan du kan lese argumenter i Bash-programmer.

# Dypdykk

Nå lurer du kanskje på hva som skjer hvis et program tar inn et ukjent antall argumenter eller om du ikke vet hvor mange argumenter som vil bli gitt. I slike tilfeller vil du kunne bruke variabelen $@ til å få tilgang til alle argumentene som ble gitt. Du kan også bruke variabelen $# til å finne ut hvor mange argumenter som ble gitt. Disse og andre variabler kan hjelpe deg med å håndtere ulike scenarier når du arbeider med argumenter i Bash-programmer.

Det er også viktig å merke seg at argumentene du leser i programmet ditt er strenger, selv om de representerer tall. Hvis du for eksempel vil bruke argumentene til å utføre en matematisk beregning, må du konvertere dem til tall først. Dette kan gjøres ved å bruke kommandoen "expr" eller ved å bruke dobbelfirkantede parenteser rundt variablene som i eksempelet over.

# Se også

- Mer informasjon om lesing av argumenter i Bash: https://www.gnu.org/software/bash/manual/html_node/Positional-Parameters.html
- Eksempler på Bash-programmer som bruker argumenter: https://www.geeksforgeeks.org/command-line-arguments-in-shell-scripting/
- Praktiske tips og triks for å arbeide med argumenter i Bash: https://medium.com/@TheKodeMan/bash-command-line-arguments-tips-tricks-a8d032a2dc4f