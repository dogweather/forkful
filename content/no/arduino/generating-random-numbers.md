---
title:    "Arduino: Generering av tilfeldige tall"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Hvorfor

Å generere tilfeldige tall er en viktig del av programmørens verktøykasse, spesielt når man jobber med Arduino. Tilfeldige tall kan brukes til å skape variasjon og realisme i spill, simuleringer, og til å bekrefte kryptografiske systemer. Det er også en nyttig verktøy for å teste programvare og maskinvare på en uforutsigbar måte.

## Hvordan

Å generere tilfeldige tall på Arduino er enkelt. Først må du sette "randomSeed ( )" funksjonen for å initialisere den tilfeldige tallgenerator. Dette gjør at generatorn bruker en annen startverdi hver gang programmet kjøres, noe som bidrar til å skape en mer tilfeldig sekvens av tall.

Deretter kan du bruke "random ( )" funksjonen til å generere tilfeldige tall mellom to bestemte verdier. For eksempel, hvis du ønsker å generere et tilfeldig tall mellom 0 og 10, kan du bruke koden:

```Arduino
int tilfeldigTall = random (0,10);
```

Dette vil returnere et tilfeldig tall mellom 0 og 9, siden "random ( )" funksjonen alltid velger et tall som er én mindre enn det øvre nummeret.

## Dypdykk

Det er viktig å merke seg at "randomSeed ( )" funksjonen bare trenger å bli kalt én gang i begynnelsen av programmet. Hvis den kalles flere ganger, vil det føre til at generatorn starter fra begynnelsen igjen og skaper en lignende sekvens av tilfeldige tall.

Det kan også være nyttig å vite at "random ( )" funksjonen bruker en pseudotilfeldig tallgenerering. Dette betyr at det ikke er helt tilfeldig, men følger et mønster som simulerer tilfeldighet. Hvis du trenger ekte tilfeldige tall, bør du vurdere å bruke en ekstern tilfeldig nummergenerator.

## Se også

- [randomSeed ( ) reference på Arduino offisiell nettside](https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
- [random ( ) reference på Arduino offisiell nettside](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)