---
title:    "Ruby: Å finne lengden av en streng"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen bruke tid på å finne lengden til en streng? Du kan tenke at det er en enkel oppgave som ikke behøver så mye innsats, men faktum er at å kunne finne lengden til en streng er en viktig og nyttig ferdighet i programmering.

## Hvordan gjøre det

For å finne lengden til en streng i Ruby, kan du bruke en innebygd metode som heter `length`. La oss se på et eksempel:

```
strek = "Hei, verden!"
puts strek.length
```

Dette vil gi følgende utskrift:

```
13
```

Her er `strek` variabelen som inneholder strengen "Hei, verden!". Når vi kaller på `length` metoden på streken variabelen, returnerer den lengden av strengen, som i dette tilfellet er 13 tegn.

Du kan også bruke `size` metoden til å finne lengden av en streng. Begge disse metodene fungerer på samme måte og gir samme resultat.

## Dypdykk

Et viktig konsept å forstå når det kommer til å finne lengden av en streng er at indekseringen begynner på 0. Det betyr at det første tegnet i en streng får indeks 0, det andre får indeks 1, og så videre. Hvis du for eksempel vil finne lengden av strengen "Hei", vil `length` metoden gi deg 3 som resultat, siden det er tre tegn i strengen.

Det er også verdt å merke seg at når du bruker Unicode-tegn i en streng, kan lengden til strengen bli annerledes enn antall tegn du faktisk ser på skjermen. Dette skyldes at Unicode-tegn har forskjellige størrelser, og derfor kan en streng med f.eks. emoji og vanlige bokstaver ha samme lengde, men forskjellig antall tegn.

## Se også

- [Ruby dokumentasjon for `length` metoden](https://ruby-doc.org/core-3.0.0/String.html#method-i-length)
- [Stack Overflow thread om forskjellen mellom `length` og `size` i Ruby](https://stackoverflow.com/questions/27347027/difference-between-length-and-size-of-an-array-in-ruby)