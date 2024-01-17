---
title:                "Store bokstaver i en tekststreng"
html_title:           "Arduino: Store bokstaver i en tekststreng"
simple_title:         "Store bokstaver i en tekststreng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hva & hvorfor?
Å kapitalisere en streng betyr å gjøre den første bokstaven i hvert ord stor. Programmere gjør dette for å gjøre teksten mer leselig og estetisk tiltalende. 

# Hvordan:
```Arduino 
String tekst = "dette er en test";
tekst.capitalize(); 
```
```Arduino
Output: Dette Er En Test
```

# Dypdykk:
Å kapitalisere en streng har vært et standardkonsept i programmering i lang tid, og det finnes mange måter å implementere dette på. Alternativene inkluderer å bruke innebygde funksjoner som "capitalize()", eller å lage din egen funksjon ved å iterere gjennom hvert ord i strengen og endre den første bokstaven til stor bokstav. 

# Se også:
For mer informasjon om å kapitalisere en streng i Arduino, se følgende kilder: 
- Arduino String objekt dokumentasjon: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Tutorial om å bruke "capitalize()" funksjonen: https://www.programmersought.com/article/62436480009/