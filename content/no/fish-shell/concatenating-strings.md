---
title:    "Fish Shell: Sammenstilling av strenger"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skulle noen bry seg med å kombinere strenger i Fish Shell? Svaret er enkelt – det er en enkel og nyttig måte å manipulere data på. Ved å kombinere strenger kan du lage mer komplekse utdata som gjenspeiler dataene du jobber med.

# Hvordan

Kombinere strenger i Fish Shell er enkelt. Du trenger bare å bruke operatoren "+" for å kombinere to strenger sammen. For eksempel:

```Fish Shell
set navn "Maria"
set etternavn "Kvamme"
echo "Hei " + $navn + " " + $etternavn 
```

Dette vil gi deg utdataen "Hei Maria Kvamme" i terminalen. Merk at du må bruke "$" foran variabelnavnet for å referere til den.

Du kan også kombinere flere strenger samtidig ved å bruke en liste av variable. For eksempel:

```Fish Shell
set tall1 "1"
set tall2 "2"
set tall3 "3"
echo $tall1 $tall2 $tall3 
```

Dette vil gi deg utdataen "1 2 3". Ved å kombinere flere strenger kan du lage komplekse utdata som fungerer bedre for dine behov.

# Deep Dive

For de som er interesserte i de tekniske detaljene, kan det være nyttig å vite at når du kombinerer strenger i Fish Shell, blir de faktisk ikke kombinert fysisk. I stedet blir det opprettet en referanse til den eksisterende strengen og variabelnavnet blir koblet sammen for å danne den nye utdataen. Dette gjør at det går raskt å manipulere og kombinere strenger uten å måtte lagre kopier av dem.

# Se også

Her er noen nyttige ressurser for å lære mer om å kombinere strenger i Fish Shell:

- [Offisiell Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Fish Shell Reddit-fellesskap](https://www.reddit.com/r/fishshell/)
- [Tutorial om å kombinere strenger i Fish Shell](https://www.baeldung.com/fish-shell-concatenate-strings)