---
title:                "Bash: Sammenligning av to datoer"
programming_language: "Bash"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Når man jobber med programmering, er det ofte nødvendig å sammenligne to forskjellige datoer. Dette kan være for å sortere data, filtrere ut gamle eller nye elementer, eller for å sjekke om en hendelse ligger før eller etter en annen. Enten du er en erfaren utvikler eller nybegynner, er det viktig å ha kunnskap om hvordan man sammenligner datoer i Bash-programmering.

## Hvordan
I Bash-programmering, kan man sammenligne datoer ved hjelp av kommandoen "timespans". Denne kommandoen lar deg sammenligne to datoer og returnere et resultat basert på forholdet mellom dem. La oss se på et eksempel:

```Bash
start_date = "2021-05-20"
end_date = "2021-05-25"

if [ "$start_date" -eq "$end_date" ]; then
    echo "Datoene er like"
elif [ "$start_date" -lt "$end_date" ]; then
    echo "Startdatoen kommer før sluttdatoen"
else
    echo "Sluttdatoen kommer før startdatoen"
fi
```

I dette eksempelet, blir datoen "start_date" sammenlignet med "end_date". Hvis de to datoene er like, vil skriptet returnere "Datoene er like", hvis startdatoen kommer før sluttdatoen vil det returnere "Startdatoen kommer før sluttdatoen", og hvis sluttdatoen kommer før startdatoen vil det returnere "Sluttdatoen kommer før startdatoen". Dette kan du bruke til å filtrere ut elementer basert på dato, eller utføre ulike handlinger avhengig av forholdet mellom to datoer.

## Deep Dive
I Bash-programmering, er datoer representert som "timespans" - en litenere enhet enn sekunder. Dette er en grunnleggende enhet som brukes til å måle tidsintervaller. Timespans kan representeres på flere forskjellige måter, og man kan endre formatet ved å bruke kommandoen "date". For mer informasjon om timespans, kan man lese mer om det her: [https://www.gnu.org/software/bash/manual/bash.html#Shell-Arithmetic](https://www.gnu.org/software/bash/manual/bash.html#Shell-Arithmetic)

## Se Også
Her er noen nyttige ressurser for å lære mer om Bash-programmering:

- Bash Manual (på norsk): [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)
- Bash Cheatsheet: [https://devhints.io/bash](https://devhints.io/bash)
- Bash scripting tutorial: [https://linuxconfig.org/bash-scripting-tutorial-for-beginners](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)

Lykke til med å sammenligne datoer i Bash-programmering!