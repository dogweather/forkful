---
title:                "Gleam: Nygenerering av tilfeldige tall"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor generate tilfeldige tall?

Generering av tilfeldige tall er en viktig del av datasamling og behandling i dagens moderne verden, og dette gjelder også i Gleam-programmering. Tilfeldige tall brukes ofte innenfor spillutvikling, simuleringer, sikkerhet og kryptografi, samt for å skape variasjon og unike brukeropplevelser. Uansett hva ditt formål er, er det avgjørende å kunne generere tilfeldige tall nøyaktig og effektivt.

## Slik gjør du det: 

For å generere tilfeldige tall i Gleam, bruker man funksjonen ```random.float()``` for å få et flyttall mellom 0 og 1, og ```random.uniform(low, high)``` for å få et flyttall mellom to spesifikke tall. Hvis du heller trenger et heltall, kan du bruke ```random.integer()```. Her er noen eksempler på kode:

```
Gleam import random

let tilfeldigTall_1 = random.float()
// tilfeldigTall_1 kan være et tall mellom 0 og 1 (for eksempel: 0.736524)

let tilfeldigTall_2 = random.uniform(1, 10)
// tilfeldigTall_2 kan være et tall mellom 1 og 10 (for eksempel: 6.2847)

let tilfeldigTall_3 = random.integer(1, 100)
// tilfeldigTall_3 kan være et heltall mellom 1 og 100 (for eksempel: 43)
```

Når du kjører denne koden, vil du få nye tilfeldige tall hver gang.

## Dykk dypere: 

Bak kulisser i Gleam brukes en algoritme kalt Mersenne Twister for å generere tilfeldige tall. Denne algoritmen er basert på matematisk teori og er kjent for å gi høy kvalitet og jevn fordeling av tilfeldige tall. Du kan også spesifisere en frøverdi for å få de samme tilfeldige tallene hver gang.

Det er også mulig å generere tilfeldige verdier basert på en liste eller et intervall, ved bruk av ```random.from_list(liste)``` eller ```random.from_range(min, max)```. Dette kan være nyttig i situasjoner der du trenger å generere tilfeldige spørsmål eller alternativer.

# Se også:

- [Gleam dokumentasjon - Random modul](https://gleam.run/modules/random)
- [Wikipedia - Tilfeldig tall generator](https://no.wikipedia.org/wiki/Tilfeldig_tallgenerator)
- [Random.org](https://www.random.org/) - en nettjeneste for å generere tilfeldige tall basert på virkelige fysiske fenomener.