---
title:    "Python: Å bruke regulære uttrykk"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Hvorfor

Mange ganger når vi jobber med tekst, har vi behov for å søke etter et bestemt mønster eller ord. Dette kan være en utfordring når vi håndterer store tekstfiler. Heldigvis finnes det et verktøy som kan gjøre denne oppgaven mye enklere - regulære uttrykk. Ved å bruke regulære uttrykk i vår Python-kode, kan vi effektivt søke etter og manipulere tekst på en rask og nøyaktig måte.

## Hvordan

For å bruke regulære uttrykk i Python, må vi først importere "re" biblioteket. Deretter kan vi bruke forskjellige metoder innenfor dette biblioteket for å søke etter og manipulere tekst.

```Python
# Importerer "re" biblioteket
import re

# Definerer en tekststreng å søke i
tekst = "Dette er en teststreng for å vise hvordan man bruker regulære uttrykk."

# Søker etter alle forekomster av ordet "teststreng"
resultat = re.findall("teststreng", tekst)

# Skriver ut resultatet
print(resultat)
```

Dette vil gi følgende utskrift: ["teststreng"]

Vi kan også bruke regulære uttrykk for å erstatte tekst i en streng. For eksempel, hvis vi ønsker å erstatte alle forekomster av ordet "teststreng" med "eksempel" i vår tekststreng, kan vi gjøre det som følger:

```Python
# Erstatter ordet "teststreng" med "eksempel"
ny_tekst = re.sub("teststreng", "eksempel", tekst)

# Skriver ut den nye teksten
print(ny_tekst)
```

Dette vil gi følgende utskrift: Dette er en eksempel for å vise hvordan man bruker regulære uttrykk.

## Dypere dykk

Regulære uttrykk kan virke overveldende ved første øyekast, men de følger et bestemt mønster som gjør dem enklere å forstå. En vanlig måte å uttrykke mønsteret på er ved hjelp av såkalte "metakarakterer". Disse representerer ikke en bestemt bokstav eller tall, men heller en type tegn eller et sett med tegn.

For eksempel, hvis vi ønsker å søke etter et ord som kan være stavet på to forskjellige måter (for eksempel "eller" og "eller"), kan vi bruke metakarakteren "+" som betyr at tegnet foran kan forekomme en eller flere ganger. Vi kan også bruke metakarakteren "*" som betyr at tegnet foran kan forekomme null eller flere ganger.

## Se også

- [Dokumentasjon for regulære uttrykk i Python](https://docs.python.org/3/library/re.html)
- [Dokumentasjon for regulære uttrykk for språket Python](https://docs.python.org/3/howto/regex.html)
- [Oppgavesett for å øve på regulære uttrykk](https://regexone.com/)