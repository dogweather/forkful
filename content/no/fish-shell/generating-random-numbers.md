---
title:    "Fish Shell: Generere tilfeldige tall"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hvorfor

Generering av tilfeldige tall er en viktig del av mange programmeringsoppgaver, enten det er for å lage løkker, utføre skytebaner, eller til og med å simulere tilfeldige hendelser. Med Fish Shell, kan du enkelt generere tilfeldige tall for å få programmene dine til å fungere enda bedre.

## Hvordan

For å generere tilfeldige tall i Fish Shell, kan du bruke kommandoen `seq` sammen med flagget `-r` for å indikere at tallene skal være tilfeldige. Her er et eksempel på hvordan du kan bruke dette i en løkke:

```Fish Shell
for tall in (seq -r 1 10)
	echo $tall
end
```

Dette vil skrive ut 10 tilfeldige tall mellom 1 og 10, som f.eks. kan være 3, 8, 4, 10, 1, 5, 7, 2, 6 og 9.

## Dypdykk

For å generere tilfeldige tall, bruker Fish Shell en algoritme kalt Mersenne Twister, som er en av de mest brukte tilfeldighetsgeneratorene i verden. Denne algoritmen genererer sekvenser av tall som ser ut til å være tilfeldige, men faktisk følger et bestemt mønster. Derfor kan ikke sekvensene av tall betraktes som virkelig tilfeldige, men de anses som god nok for de fleste programmeringsformål.

## Se Også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/commands.html#seq)
- [Mersenne Twister algoritme](https://en.wikipedia.org/wiki/Mersenne_Twister)