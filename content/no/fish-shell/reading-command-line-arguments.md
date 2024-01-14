---
title:    "Fish Shell: Lese inn kommandolinje argumenter"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Hvorfor
Velkommen til vår blogg om Fish Shell programmering! Hvis du er en norsk bruker som ønsker å dykke inn i verden av kommandolinjemiljøer, har du kommet til rett sted. I dag skal vi diskutere hvordan man kan lese kommandolinjeargumenter ved hjelp av Fish Shell.

## Slik gjør du det
For å lese kommandolinjeargumenter ved hjelp av Fish Shell, må du først forstå syntaxen for å lese argumentene. Dette gjøres ved å bruke spesialvariabelen `$argv`. La oss se på et eksempel:

```
Fish Shell 'set argumenter Eco'
echo $argv
```

Dette vil returnere en liste over alle argumentene du har gitt til kommandoen, i dette tilfellet `"Eco"`, og vil være nyttig for senere bruk i din programmering.

## Dypdykk
For å forstå hvordan Fish Shell leser kommandolinjeargumenter, er det viktig å forstå den underliggende mekanismen for å tolke argumentene. Fish Shell bruker en "lazy loading" strategi når det kommer til parsing av argumentene, noe som betyr at i motsetning til andre kommandolinjemiljøer, vil Fish Shell bare lese argumentene når de skal brukes istedenfor å laste dem alle inn i minnet hver gang kommandoen kjøres.

Dette gir fordeler når det kommer til effektivitet og hastighet, men også noen ulemper, som for eksempel når argumentene trenger å bli filtrert eller transformert før kjøring av kommandoen. I slike tilfeller anbefales det å bruke Fish Shells innebygde funksjoner som `string split` for å håndtere argumentene på en mer kontrollert måte.

## Se også
For mer informasjon om å lese kommandolinjeargumenter i Fish Shell, se følgende ressurser:

- [Fish Shell dokumentasjon om kommandolinjeargumenter](https://fishshell.com/docs/current/commands.html#sub-shells) 
- [En tutorial på å lese kommandolinjeargumenter i Fish Shell](https://scotch.io/tutorials/getting-started-with-fish-the-friendly-interactive-shell)
- [Hvordan bruke Fish Shell som din daglige kommandolinje](https://opensource.com/article/18/6/using-fish-instead-bash)

Vi håper du har funnet denne artikkelen nyttig og at du er inspirert til å utforske mer av Fish Shell programmering! Lykke til og ha det gøy på kommandolinjen!