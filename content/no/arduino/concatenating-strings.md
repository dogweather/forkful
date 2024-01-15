---
title:                "Sammenslåing av strenger"
html_title:           "Arduino: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skal man egentlig bruke concatenating strings i Arduino-programmering? La oss se nærmere på hva dette betyr og hvordan det kan være nyttig.

Arduino-programmering innebærer å skrive kode som kan utføre ulike handlinger ved hjelp av mikrokontrolleren. I noen tilfeller, spesielt når vi jobber med tekstbasert informasjon, kan det være nødvendig å kombinere flere ord eller setninger for å oppnå det ønskede resultatet. Her kommer concatenating strings inn i bildet.

# Hvordan gjør man det

Hvis vi vil slå sammen to eller flere tekststrenger, kan vi bruke pluss-tegnet (+) til å kombinere dem. La oss se på et enkelt eksempel:

```
ArduinoSerial.begin(9600); 
String navn = "Magnar"; 
String melding = "Hei, " + navn + "!"; 
Serial.println(melding);
```

I dette eksempelet bruker vi concatenate strings for å lage en hilsen til personen med navnet "Magnar". Når vi kjører koden, vil vi se følgende utskrift på serieporten: Hei, Magnar!

Vi kan også bruke concatenate strings for å legge til tall eller andre variabler i en tekststreng. Her er et annet eksempel som viser hvordan vi kan bruke denne teknikken til å lage en kompleks melding:

```
ArduinoSerial.begin(9600); 
int poeng = 80; 
String melding = "Du fikk " + String(poeng) + " poeng!"; // String(poeng) konverterer tallet til en tekststreng 
Serial.println(melding);
```

I dette tilfellet vil koden skrive ut følgende melding: Du fikk 80 poeng!

Husk at når vi jobber med concatenate strings, må vi sørge for å inkludere mellomrom og andre tegn der det er nødvendig for å få en lesbar utskrift.

# Dypdykk

Når vi bruker concatenate strings i Arduino, opprettes en midlertidig tekststreng hver gang vi kombinerer to strenger. Dette kan legge til ekstra arbeidsminne og dermed påvirke ytelsen til koden vår. Derfor er det viktig å være selektiv med bruken av denne teknikken og unngå å slå sammen for mange strenger i en enkelt kodeblokk.

Vi bør også være forsiktige med å bruke concatenate strings i en løkke, da dette kan føre til ytelsesproblemer. Det er også viktig å merke seg at concatenate strings bare fungerer for tekststrenger og ikke tall og andre variabler.

# Se også

Her er noen nyttige lenker for å lære mer om concatenate strings i Arduino-programmering:

- Offisiell Arduino-veiledning for bruk av concatenate strings: https://www.arduino.cc/reference/en/language/functions/communication/serial/print/
- En god artikkel om å optimalisere concatenate strings for bedre ytelse: https://forum.arduino.cc/index.php?topic=524169.0
- En rask referanseguide for bruk av concatenate strings i Arduino: https://learn.sparkfun.com/tutorials/arduino-104-string-append-operator/data-types-strings