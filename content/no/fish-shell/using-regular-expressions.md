---
title:                "Fish Shell: Å bruke regulære uttrykk"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hvorfor

Regular expressions, eller regulære uttrykk, er et kraftig verktøy for tekstbehandling og datahåndtering. Det kan hjelpe deg med å søke, filtrere og manipulere tekst på en mer avansert og effektiv måte. Dette kan være spesielt nyttig for utviklere, dataanalytikere og andre som jobber med store mengder tekst og data.

# Hvordan

For å bruke regulære uttrykk i Fish Shell, må du først forstå grunnstrukturer som brukes til å representere tekstmønstre. La oss for eksempel si at vi vil finne alle e-postadresser i en tekstfil.

```
Fish Shell kodeblokk:

set emails (pcregrep -o "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,6}" file.txt)
echo $emails
```

I dette eksempelet bruker vi kommandoen "pcregrep" som står for "Perl Compatible Regular Expression grep". Vi oppgir regex-mønsteret som skal matches, i dette tilfellet en standard e-postadresse. Deretter bruker vi uttrykket "set emails" for å lagre resultatet i en variabel, som vi kan bruke senere. Til slutt skriver vi ut alle e-postadressene ved å bruke kommandoen "echo" og variabelen vår.

# Dypdykk

Regulære uttrykk kan være ganske komplekse og ha mange forskjellige muligheter. Her er noen tips for å hjelpe deg med å bli mer effektiv med regex i Fish Shell:

- Bruk Flag-completion: Fish Shell har en innebygd funksjon for å fullføre valgflagg ved hjelp av "Tab"-tasten, noe som gjør det enklere å huske alle mulighetene.

- Bruk forsvarlig: Dette vil vise deg et vindu med alle resulterende treff for et gitt regex-mønster, slik at du kan inspisere og bekrefte resultatene før du fortsetter med behandlingen.

- Bruk regex tester: Det er mange online verktøy du kan bruke til å teste og feilsøke regex-mønstrene dine før du bruker dem i Fish Shell.

# Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Regulære uttrykk referanse](https://www.regular-expressions.info/)
- [Fish Shell - Nyttige triks og tips](https://fishshell.com/docs/current/tutorial.html)