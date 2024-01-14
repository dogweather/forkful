---
title:    "Bash: Sammenligner to datoer"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor 

Å sammenligne to datoer er en viktig del av Bash-programmering, spesielt hvis du jobber med datoer og klokkeslett. Det kan hjelpe deg med å finne ut hvilken dato som kommer først eller sist, og du kan bruke denne informasjonen til å organisere filer eller generere rapporter. Det er også nyttig hvis du ønsker å sjekke om en bestemt dato allerede har passert eller når en bestemt hendelse vil skje.

## Slik gjør du det 

Bash har innebygde funksjoner for å sammenligne datoer, og det er enkelt å bruke dem i dine egne skript. Her er noen eksempler på hvordan du kan sammenligne to datoer i Bash:

```Bash 
#!/bin/bash 
first_date="2020-01-05" 
second_date="2019-12-31" 

if [[ "$first_date" > "$second_date" ]] 
then 
    echo "$first_date kommer etter $second_date" 
else 
    echo "$first_date kommer før $second_date" 
fi 
```

I dette eksemplet har vi to variabler som inneholder datoer og en if-setning som sammenligner dem ved hjelp av operatorer for større enn og mindre enn. Når du kjører dette skriptet, vil du få følgende utgang:

```
2020-01-05 kommer etter 2019-12-31
```

Du kan også bruke samme tilnærming for å sammenligne klokkeslett. Her er et annet eksempel:

```Bash
#!/bin/bash 
first_time="10:00" 
second_time="12:30"

if [[ "$first_time" > "$second_time" ]] 
then 
    echo "$first_time kommer etter $second_time" 
else 
    echo "$first_time kommer før $second_time" 
fi 
```

Dette skriptet vil returnere følgende utgang:

```
10:00 kommer før 12:30
```

## Dypdykk 

Bash bruker sekunder siden epoch (1. januar 1970) for å sammenligne datoer og klokkeslett. Det betyr at hvis du sammenligner en dato og et klokkeslett, vil Bash behandle dem som sekunder siden epoch. Dette kan føre til uventede resultater, så det er viktig å være klar over dette når du arbeider med datoer og klokkeslett.

Du kan også bruke variablen "date" i Bash til å generere datoer og utføre operasjoner på dem. For å få dagens dato, kan du kjøre følgende kommando i terminalen:

```
date +"%Y-%m-%d"
```

Dette vil returnere dagens dato i formatet år-måned-dag.

## Se også 

For mer informasjon om å sammenligne datoer i Bash, kan du sjekke ut følgende ressurser:

- [Bash sekunder siden epoch](https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man3/date.3.html)
- [Bash innebygde funksjoner](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Builtins)
- [Date-kommandoen](https://www.gnu.org/software/coreutils/manual/html_node/Date.html)

Jeg håper denne artikkelen har hjulpet deg med å forstå hvordan du kan sammenligne datoer i Bash. Lykke til med dine fremtidige programmeringsprosjekter!