---
title:    "Bash: Utskrift av feilsøkingsutdata"
keywords: ["Bash"]
---

{{< edit_this_page >}}

##Hvorfor
Debugging er en viktig del av enhver programmeringsoppgave. Å kunne forstå koden din og feilsøke effektivt er avgjørende for å skrive gode og fungerende programmer. En nyttig teknikk i denne prosessen er å skrive ut feilmeldinger og variabler til terminalen, også kjent som "debug output". Dette kan hjelpe deg med å identifisere problemområder i koden din og finne feil raskere. I denne bloggposten vil jeg vise deg hvordan du enkelt kan legge til debug output i Bash-skriptene dine.

##Slik gjør du det
For å legge til debug output i Bash-skriptene dine, kan du bruke kommandoen "echo". Dette vil skrive ut en melding eller en variabel til terminalen når skriptet ditt kjører.

```Bash
#!/bin/bash
# Definer en variabel
navn="Petra"

# Skriv ut variabelen ved hjelp av echo
echo "Hei, mitt navn er $navn"
```

Kjører du dette skriptet vil du få følgende utskrift:

```
Hei, mitt navn er Petra
```

Som du kan se, kan du enkelt skrive ut variabler ved å bruke "echo" -kommandoen og inkludere variabelen ved hjelp av "$"-tegnet. Dette kan være nyttig når du ønsker å sjekke verdien av variabler i løpet av skriptet ditt.

Du kan også bruke "echo" til å skrive ut feilmeldinger når noe går galt i koden din. Dette kan hjelpe deg med å identifisere hvor og hvorfor feilen skjedde.

```Bash
#!/bin/bash
# Lager en variabel med feil verdi
alder=-5

# Sjekk om alderen er et positivt tall
if [ $alder -lt 0 ]; then
  echo "Feil: Alderen kan ikke være et negativt tall."
fi
```

Når du kjører dette skriptet, vil du få følgende feilmelding:

```
Feil: Alderen kan ikke være et negativt tall.
```

Dette vil hjelpe deg med å identifisere problemet og dermed feilsøke mer effektivt.

##Dypdykk
I tillegg til å bruke "echo" som en enkel måte å legge til debug output i Bash-skriptene dine, kan du også eksperimentere med å bruke andre kommandoer som "printf" og "cat". Disse kan gi mer formatert og spesifikk utskrift av variabler og feilmeldinger.

Det kan også være nyttig å lese dokumentasjonen på kommandoene du bruker for å lære mer om deres muligheter og funksjonalitet.

##Se også
- [Bash Guide for nybegynnere](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [10 Tips for feilsøking i Bash](https://www.tecmint.com/debugging-bash-scripts/)
- [Grundig Bash-skriving](https://www.highwaydata.com/tutorials/Tutorial_bash.txt)