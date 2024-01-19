---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å tolke en dato fra en streng betyr å omdanne den lesbare tekstformen til en datoverdi for datohåndtering. Programmerere gjør dette for å utføre tidsspesifikke operasjoner, som å beregne tiden mellom datoer eller håndtere datoformat på tvers av forskjellige tidssoner.

## Hvordan Gjøre:

Her er en grunnleggende Arduino-kode for datoparsing fra en streng.

```Arduino
#include <TimeLib.h> //Dette biblioteket åpner for tid og dato funksjoner.
  
String datoStreng = "26/11/2020"; //Definerer dato som en streng.
  
void setup() {
  
  Serial.begin(9600); //Start seriell kommunikasjon med en baudrate på 9600.
  
  int dag = datoStreng.substring(0,2).toInt(); //Hent dag.
  int mnd = datoStreng.substring(3,5).toInt(); //Hent måned.
  int år = datoStreng.substring(6,10).toInt(); //Hent år.
  
  setTime(0, 0, 0, dag, mnd, år); //Sett tiden ved hjelp av den tolkede datoen. 
  Serial.println(day());  //Skriv ut dagen.
  Serial.println(month()); //Skriv ut måneden.
  Serial.println(year()); //Skriv ut året.
}
  
void loop() {
  
  //La denne plassen være fri.
}
```
## Dyp Dykk:

Å tolke datoer fra strenger har blitt brukt siden tidlige programmeringsspråk som COBOL og Fortran. Det krever ikke bare å omdanne representasjonen, men også å håndtere skuddår og forskjellige kalenderformater.

En alternativ måte er å bruke funksjonen `strptime()` som er tilgjengelig i noen C/C++ bibliotek. Men det er ikke alltid tilgjengelig på Arduino plattformen. Dessuten, for svært begrensete systemer som Arduino, kan manuell parsing være mer ressurseffektiv.

Implementeringsdetaljer kan variere basert på strengformatet. Typiske datoformater inkluderer "MM/DD/ÅÅÅÅ", "DD-MM-ÅÅÅÅ" eller "ÅÅÅÅ/MM/DD". Din kode må kunne håndtere det formatet du forventer.

## Se Også:

1. [Time Arduino Library](https://www.arduino.cc/reference/en/libraries/time/) - Bibliotek for tid- og datohåndtering.
2. [Arduino String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/) - For mer informasjon om strengobjekter i Arduino. 
3. [Arduino Reference](https://www.arduino.cc/reference/en/) - Den offisielle referansemanualen, med detaljer om innebygde funksjoner.