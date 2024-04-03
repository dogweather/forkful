---
date: 2024-01-27 20:33:33.544070-07:00
description: "Hvordan: \xC5 generere et tilfeldig tall i Fish kan v\xE6re enkelt,\
  \ ved \xE5 bruke kombinasjonen av systemverkt\xF8y og skallfunksjoner. Nedenfor\
  \ er noen eksempler\u2026"
lastmod: '2024-03-13T22:44:41.220650-06:00'
model: gpt-4-0125-preview
summary: "\xC5 generere et tilfeldig tall i Fish kan v\xE6re enkelt, ved \xE5 bruke\
  \ kombinasjonen av systemverkt\xF8y og skallfunksjoner."
title: Generering av tilfeldige tall
weight: 12
---

## Hvordan:
Å generere et tilfeldig tall i Fish kan være enkelt, ved å bruke kombinasjonen av systemverktøy og skallfunksjoner. Nedenfor er noen eksempler som demonstrerer hvordan man genererer tilfeldige tall innenfor spesifiserte områder.

**Generer et tilfeldig tall mellom 0 og 100:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**Eksempel på utskrift:**
```fish
42
```

**Generere et tilfeldig tall mellom to hvilke som helst tall, si 50 og 150:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**Eksempel på utskrift:**
```fish
103
```

**Bruke random til å stokke en liste:**

Du vil kanskje også tilfeldig stokke elementene i en liste. Her er hvordan du kan gjøre det:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**Eksempel på utskrift:**
```fish
C
A
E
D
B
```

Vennligst merk, utskriften vil variere hver gang du kjører disse kommandoene på grunn av tilfeldighetens natur.

## Dypdykk
Fish Shell `random`-funksjonen gir et brukervennlig grensesnitt for å generere pseudo-tilfeldige tall. Internt pakker den rundt systemnivå tilfeldig tallgenerering verktøy, og tilbyr en bærbar måte å introdusere tilfeldighet i skriptene dine på. Det er imidlertid viktig å huske på at tilfeldigheten levert av `random` er tilstrekkelig for de fleste skriptoppgaver, men kanskje ikke oppfyller kravene til kryptografisk sikkerhet for applikasjoner som trenger en høyere grad av uforutsigbarhet.

For kontekster med høy sikkerhet, vurder å bruke spesialiserte verktøy eller programmeringsbiblioteker designet for kryptografiske formål, som gir sterkere garantier for tilfeldighet. Likevel, for generell skripting og applikasjoner hvor de høyeste sikkerhetsstandardene for tilfeldighet ikke er et krav, tilbyr Fish Shell's `random` funksjon en praktisk og effektiv løsning.
