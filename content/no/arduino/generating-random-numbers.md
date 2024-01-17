---
title:                "Generering av tilfeldige tall"
html_title:           "Arduino: Generering av tilfeldige tall"
simple_title:         "Generering av tilfeldige tall"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor? 
Generering av tilfeldige tall er en viktig del av programmering, da det tillater oss å skape variabler og situasjoner som ikke er forutsigbare. Dette gjør programmer mer dynamiske og realistiske.

# Hvordan: 
Det er enkelt å generere tilfeldige tall ved hjelp av Arduino. Her er et eksempel på hvordan du kan lage og skrive ut et tilfeldig tall mellom 1 og 10:

```Arduino
int tilfeldigTall = random(1, 11); //genererer et tall mellom 1 og 10
Serial.println(tilfeldigTall); //skriver ut det tilfeldige tallet i seriell monitor
```

Dette kodeeksempelet bruker funksjonen "random", som tar inn to argumenter - minimumsverdi og maksimumsverdi - og deretter genererer et tilfeldig tall innenfor dette området. Du kan også bruke denne funksjonen for å generere tilfeldige tall for andre typer variabler, for eksempel float eller char.

# Deep Dive: 
Generering av tilfeldige tall har vært en viktig del av programmering siden begynnelsen. Det er flere måter å generere tilfeldige tall på, for eksempel bruk av matematiske formeler eller bruk av fysiske fenomener som for eksempel termisk støy. Arduino bruker en pseudo-tilfeldig tallgenerator som bruker en algoritme for å skape en sekvens av tall som virker tilfeldige.

Det finnes også alternative biblioteker som kan brukes i stedet for den innebygde "random" funksjonen, som for eksempel "randomSeed" som tillater deg å bruke eksterne innganger (som for eksempel en analog pinne) for å generere tilfeldige tall.

# Se også: 
- Offisiell dokumentasjon for "random" funksjonen: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- En guide til generering av tilfeldige tall på Arduino: https://www.arduino.cc/reference/en/libraries/random/