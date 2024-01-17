---
title:                "Interpolering av en streng."
html_title:           "Arduino: Interpolering av en streng."
simple_title:         "Interpolering av en streng."
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Interpolering av en streng i programmering er når man bygger en ny streng ved å kombinere deler fra ulike strenger på en mer effektiv måte. Dette kan være nyttig for å lage mer dynamiske og tilpassede meldinger eller brukerinnputt i et program.

## Slik gjør du det:
Arduino har innebygde funksjoner for å utføre strenginterpolering, som `String()` og `StringFormat()`. Disse lar deg enkelt kombinere variabler, tall og tekststrenger til en ny streng. Her er et eksempel for å lage en personlig melding:

```arduino
int alder = 28;
String melding = String("Hei! Jeg er ") + String(alder) + String(" år gammel.");
Serial.println(melding); //Output: Hei! Jeg er 28 år gammel.
```

## Dykk dypere:
Strenginterpolering har eksistert siden programmeringens tidligste dager, men har blitt mer populært med introduksjonen av språk som Ruby og Python. Alternativer til de innebygde funksjonene i Arduino inkluderer å bruke tredjeparts bibliotek eller tilpasse egne funksjoner. Det er også viktig å være oppmerksom på potensielle sikkerhetsrisikoer når du bruker interpolering av brukerinnputt i et program.

## Se også:
- [String dokumentasjon](https://www.arduino.cc/reference/en/language/variables/data-types/string/) fra Arduino
- [En dypere forklaring av interpolering](https://www.thoughtco.com/what-is-string-interpolation-2034168)
- [Sikkerhetshensyn når du bruker interpolering](https://security.stackexchange.com/questions/42419/is-string-interpolation-a-security-vulnerability)