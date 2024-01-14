---
title:    "Fish Shell: Skriver en tekstfil"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor
Hvis du er en programmerer eller en systemadministrator, har du sannsynligvis allerede erfaring med å skrive skript for å automatisere ulike oppgaver på datamaskinen din. Men hva med en enklere og mer brukervennlig måte å håndtere disse oppgavene på? Det er der Fish Shell kommer inn i bildet. I stedet for å skrive kryptiske kommandoer i et tradisjonelt terminalvindu, tillater Fish Shell deg å skrive mer naturlige og leselige kommandoer ved hjelp av tekstfiler.

## Hvordan
For å skrive en tekstfil i Fish Shell, må du åpne opp en tekstredigerer og skrive inn kommandoene dine. For eksempel, la oss si at du vil lage en tekstfil som lister opp alle filene som inneholder ordet "fish" i navnet. Du kan skrive følgende kommando i tekstfilen din:

```Fish Shell
ls | grep -i "fish"
```

Deretter må du lagre tekstfilen med et passende navn og legge til ".fish" på slutten for å indikere at det er en Fish Shell tekstfil. Så enkelt som det, du har nå en tekstfil som kan kjøres som et skript ved hjelp av Fish Shell.

## Dypdykk
En av de største forskjellene mellom Fish Shell og andre tradisjonelle skall er måten den håndterer variabler på. I stedet for å bruke syntaksen "$VAR" for å referere til en variabel, kan du bruke mer leselige og intuitive variabelnavn. For eksempel, hvis du vil sette variabelen "navn" til "Jan", kan du skrive følgende kommando:

```Fish Shell
set navn Jan
```

Fish Shell har også en rekke nyttige funksjoner som auto-fullføring, fargekoding av kommandoer, og evnen til å holde oversikt over din kommandohistorikk selv etter at du har lukket terminalvinduet. Utforsk disse funksjonene og oppdag hvorfor Fish Shell er en favoritt blant mange utviklere og sysadmins.

## Se også
- [Offisiell nettside for Fish Shell](https://fishshell.com/)
- [En enkel guide til å komme i gang med Fish Shell](https://dev.to/alpha0010/how-to-get-started-with-fish-shell-442k)
- [10 grunner til hvorfor du bør prøve Fish Shell](https://www.tecmint.com/reasons-to-switch-to-fish-shell/)