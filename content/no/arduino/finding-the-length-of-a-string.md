---
title:                "Å finne lengden av en streng"
html_title:           "Arduino: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

#Hva og hvorfor?
Det å finne lengden av en streng er å bestemme hvor mange tegn den inneholder. Dette er nyttig for å kunne manipulere og behandle tekster på en effektiv måte. 

#Hvordan:
```
Arduino String streng = "Hei, verden!"; 
int lengde = streng.length(); 

Serial.println(lengde); 
```

Output:
```
13
```

#Dypdykk:
Å finne lengden av en streng er en viktig del av tekstbehandling i programmering og har vært en utfordring siden de tidligste databehandlingsdagene. Alternativene for å finne lengden av en streng er å bruke en loop som telleverktøy eller bruke en innebygd funksjon som `length()`. Implementasjonen av `length()` funksjonen i Arduino inkluderer blant annet gyldige tegn, hvilket betyr at noen språk kan ha en annen størrelse enn andre. 

#Se også:
- [Offisiell Arduino dokumentasjon om strings](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [En dypere forklaring av strenger og hvordan å arbeide med dem](https://www.arduino.cc/en/Tutorial/StringIndexOf)