---
title:                "Fish Shell: Få dagens dato"
programming_language: "Fish Shell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Hvorfor

Å kunne hente den nåværende datoen er en viktig del av å programmere i Fish Shell. Enten du trenger det for å logge når et skript ble kjørt, eller for å organisere filer i mapper basert på opprettelsesdato, er det nyttig å vite hvordan man henter den nåværende datoen.

# Hvordan

Det er flere måter å hente den nåværende datoen på i Fish Shell, men her skal vi se på de to mest vanlige.

Først kan vi bruke kommandoen "date" og pipe outputet til "cut" for å kun få dato-delen ut. Her er et eksempel på hvordan dette kan gjøres i en Terminal:

```Fish Shell
date | cut -d' ' -f2-4
```

Dette vil gi deg output som ser slik ut:

```Fish Shell
Sep 22 2021
```

En annen måte å hente den nåværende datoen på er ved å bruke Fish Shells innebygde funksjon "fish_date". Dette vil gi deg output i et spesifikt format basert på datoen. For eksempel:

```Fish Shell
fish_date +%d.%m.%y
```

Dette vil gi deg output som ser slik ut:

```Fish Shell
22.09.21
```

# Dypdykk

Det er verdt å merke seg at Fish Shell også har en innebygd variabel som heter "status", som kan brukes til å hente forskjellig informasjon, inkludert den nåværende datoen. For eksempel:

```Fish Shell
echo $status
```

Dette vil gi deg output i et format som ser slik ut:

```Fish Shell
Jul 15 09:10:38
```

Du kan også bruke kommandoen "env" for å se en liste over alle variablene i Fish Shell og deres innhold, inkludert "status" og "fish_date".

# Se også

Her er noen nyttige lenker for å lære mer om å jobbe med datoer i Fish Shell:

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub repository](https://github.com/fish-shell/fish-shell)
- [Forum for Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Offisiell Fish Shell Slack community](https://slackin.fishshell.com/)