---
title:    "Bash: Oppretting av midlertidig fil"
keywords: ["Bash"]
---

{{< edit_this_page >}}

# Hvorfor?

Når du jobber med Bash-programmering, kan det være veldig nyttig å kunne opprette midlertidige filer. Disse filene opprettes og brukes midlertidig mens programmet kjører, og slettes deretter automatisk når programmet er ferdig. Dette kan være nyttig for å lagre midlertidige data, behandle store filer eller for å unngå overbelastning av systemet med unødvendige filer.

# Slik oppretter du en midlertidig fil

For å opprette en midlertidig fil i Bash, kan du bruke kommandoen "mktemp". Denne kommandoen genererer en unik filnavn og oppretter en tom fil med dette navnet. Du kan også spesifisere en prefiks eller suffiks for filnavnet. Se et eksempel nedenfor:

```Bash
filnavn=$(mktemp testfile.XXX)
echo $filnavn
```

Dette vil opprette en tom fil med navnet "testfile.XXX", der XXX er en tilfeldig kombinasjon av bokstaver og tall. Hvis du ønsker å endre prefikset eller suffikset, kan du legge til det som et argument til "mktemp" kommandoen.

# Dypdykk

Det er viktig å merke seg at "mktemp" ikke oppretter filen på et trygt og sikkerhetsmessig måte. Derfor er det viktig å følge noen sikkerhetsprinsipper når du bruker midlertidige filer. For eksempel kan du sette begrensninger på hvem som har tilgang til filen, eller begrense skrivetilgangen til andre brukere. Du kan også spesifisere at filen skal slettes ved en gitt hendelse, som for eksempel ved avslutningen av programmet.

# Se også

- https://linux.die.net/man/1/mktemp
- https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html
- https://bash.cyberciti.biz/guide/Cleaning_Up_Temp_Files