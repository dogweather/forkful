---
title:    "Bash: Å få gjeldende dato"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Hvorfor

Har du noen gang lurt på hvordan datamaskinen din vet hvilken dato det er i dag? Eller hvordan du kan få den til å vise deg nåværende dato og tid i terminalen? I denne bloggposten vil vi introdusere deg for noen enkle kommandoer i Bash som gjør nettopp det.

## Hvordan gjøre det

For å få nåværende dato og tid i terminalen, kan du bruke kommandoen `date`. Denne kommandoen vil vise deg dato og tid i det vanlige formatet, som for eksempel "Ons Jul 21 12:21:03 CEST 2021". Men hvis du ønsker å formatere utdataen på en annen måte, kan du legge til noen flagg etter kommandoen.

La oss si at du ønsker å få ut kun dagnummeret og måneden på formatet DD/MM. Da kan du bruke følgende kommando: 

```Bash 
date +"%d/%m"
```

Dette vil gi deg et resultat som for eksempel "21/07". Du kan også legge til tidsinformasjon ved å bruke flagget `-I` og deretter velge hvilken format du vil ha på tiden. For eksempel vil `date -I="%H:%M"` vise tiden på formatet TT:MM, som for eksempel "12:25".

Det finnes også en rekke andre flagg og formateringsalternativer som du kan lese om i `date` sin manual ved å kjøre kommandoen `man date` i terminalen.

## Dykk dypere

Når du kjører kommandoen `date`, henter den informasjon fra systemets innebygde klokke. Denne klokken er en del av maskinens hardware og holder styr på tiden uansett om den er koblet mot internett eller ikke. Et viktig poeng å merke seg er at denne klokken bruker UTC (koordinert universaltid) som standard, som kan være annerledes enn din lokaltidssone.

Dette kan du se ved å kjøre kommandoen `date -u` som vil vise deg tiden i UTC. For å få datoen og tiden på din lokaltidssone, trenger du å legge til flagget `-Isetigen m "<din tidsone>"`, for eksempel `date -Isetigen m "Europe/Oslo"`. Dette vil konvertere tiden til din tidsone.

## Se også

- [Bash-kommandoer for å håndtere dato og tid](https://www.tecmint.com/working-with-dates-time-in-linux-bash-scripts/)
- [Eksempler på bruk av `date`-kommandoen](https://www.computerhope.com/unix/udate.htm)
- [En oversikt over formatteringsalternativene til `date`-kommandoen](https://man7.org/linux/man-pages/man1/date.1.html)