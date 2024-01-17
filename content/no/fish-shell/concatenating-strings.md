---
title:                "Sammenstilling av strenger"
html_title:           "Fish Shell: Sammenstilling av strenger"
simple_title:         "Sammenstilling av strenger"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Sammenkobling av strenger, også kjent som concatenation, er når man slår sammen to eller flere strenger til en enkelt streng. Dette kan være nyttig for å lage mer komplekse tekster eller variabler i koden din. Programmører gjør det ofte for å lage dynamiske meldinger, konfigurasjonsvariabler og mye mer.

# Slik gjør du det:

```Fish Shell``` har innebygde funksjoner for å håndtere sammenkobling av strenger. La oss se på et enkelt eksempel:

```fish
set message "Hei, verden!"
echo $message
```

I dette eksempelet, lager vi en variabel kalt "message" som inneholder teksten "Hei, verden!" og bruker deretter "echo" funksjonen for å skrive ut teksten i terminalen. Du kan også kombinere flere variabler eller tekststrenger ved hjelp av "string replace" funksjonen:

```fish
set name "Ole"
set message "Hei, $name!"
string replace -r "Hei," "" $message
```

Dette vil resultere i utskriften "Ole!". Som du kan se, brukte vi "$" tegnet for å indikere at teksten som skal settes inn kommer fra en variabel. Dette er en viktig del av konkatenering i ```Fish Shell```.

# Dykk dypere:

Sammenkobling av strenger har eksistert siden begynnelsen av programmering og er en grunnleggende del av de fleste programmeringsspråk. Alternativene for sammenkobling kan variere, men i ```Fish Shell```, kan du bruke "string join" eller "printf" funksjonene til å oppnå samme resultat som "string replace" funksjonen.

Når det gjelder implementering, bruker ```Fish Shell``` "string interpolation" for å erstatte variabler i en streng mens den er i ferd med å bli utskrevet. Dette gjør det lettere og raskere enn mange andre programmeringsspråk som krever at variabler må konkatenere først før de kan bli utskrevet.

# Se også:

- Offisiell dokumentasjon for ```Fish Shell``` sin string handling: https://fishshell.com/docs/current/index.html#string-handling
- Enkel forklaring av concatenering med eksempler på flere programmeringsspråk: https://www.geeksforgeeks.org/string-concatenation-in-various-programming-languages/