---
title:                "Fish Shell: Generere tilfeldige tall"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor

Når man jobber med programmering, kan det ofte være behov for å generere tilfeldige tall. Dette kan være nyttig for ulike formål, som å simulere ulike scenarier i et spill eller å generere tilfeldige data for testing av algoritmer. Uansett hva grunnen måtte være, kan Fish Shell være en effektiv måte å generere tilfeldige tall på.

# Hvordan

For å generere tilfeldige tall i Fish Shell, kan du bruke kommandoen ```fish_random```. Denne kommandoen tar et argument som representerer det største tallet som kan genereres. La oss si at vi ønsker å generere et tilfeldig tall mellom 1 og 100, da kan vi skrive følgende i terminalen:

```
fish_random 100
```

Dette vil gi oss et tilfeldig tall mellom 1 og 100 som output. Vi kan også lagre dette tallet i en variabel ved å bruke ```set``` kommandoen, slik som dette:

```
set tall (fish_random 100)
```

Nå er tallet lagret i variabelen "tall" og kan brukes videre i koden.

# Dypdykk

Fish Shell bruker en algoritme som heter "Mersenne Twister" for å generere tilfeldige tall. Dette er en av de mest brukte algoritmene for generering av tilfeldige tall, og den har en syklus på hele 2^19937-1 tall, noe som betyr at den gir en god variasjon av tall. Denne algoritmen tar også en "seed" som input, som kan brukes til å få samme sekvens av tilfeldige tall ved hver kjøring av koden.

# Se også

- [Dokumentasjon for Fish Shell](https://fishshell.com/docs/current/)
- [Mersenne Twister algoritmen](https://en.wikipedia.org/wiki/Mersenne_Twister)
- [Eksempler på bruk av tilfeldige tall i programmering](https://www.geeksforgeeks.org/generate-random-numbers-in-programming/)