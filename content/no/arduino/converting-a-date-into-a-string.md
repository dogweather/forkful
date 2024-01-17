---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Enkel Date-Konvertering i Arduino
## Hva & Hvorfor?
 
Konvertering av datoer til strenger er en vanlig operasjon i programmering. Dette betyr rett og slett å ta en dato i en bestemt format og gjøre den om til en streng (en tekststreng). Programmere gjør dette for å kunne vise datoer leselig for brukere eller for å kunne lagre dem i en database eller fil. 
 
## Slik gjør du det
 
For å konvertere en dato til en streng i Arduino, bruker vi funksjonen ```String()```. Denne funksjonen konverterer automatisk en variabel eller et tall til en streng. Her er et eksempel på hvordan du kan bruke funksjonen i ditt Arduino-program:
 
```
// Opprett en variabel med dagens dato
int dato = 7;
// Konverter datoen til en streng og lagre den i en variabel
String dato_streng = String(dato);
```
 
Nå vil variabelen `dato_streng` inneholde strengen "7". Du kan også legge til flere variabler eller tekststrenger sammen ved hjelp av funksjonen. Her er et eksempel på hvordan du kan få en fullstendig dato og klokkeslett som en streng:
 
```
// Opprett variabler med dagens dato og klokkeslett
int dag = 7;
int måned = 11;
int år = 2021;
int time = 13;
int minutt = 30;
int sekund = 45;
// Konverter dato og klokkeslett til en streng
String tidspunkt = String(dag) + "/" + String(måned) + "/" + String(år) + " " + String(time) + ":" + String(minutt) + ":" + String(sekund);
```

Output i Serial monitor:
```
7/11/2021 13:30:45
```
 
## Dykk dypere
 
### Historisk kontekst
 
Konvertering av datoer til strenger er en vanlig operasjon i de fleste programmeringsspråk, inkludert Arduino sine egne språk som baserer seg på C og C++. Dette er en viktig funksjon for å kunne håndtere og vise datoer på en forståelig måte.
 
### Alternativer
 
En alternativ metode for å konvertere datoer til strenger i Arduino er ved hjelp av funksjonen `sprintf()`. Denne funksjonen lar deg definere et utdataformat og fylle det med variabler og tekststrenger. Du kan lese mer om denne metoden i Arduino sin [dokumentasjon](https://www.arduino.cc/reference/en/language/functions/communication/sprintf/).
 
### Implementeringsdetaljer
 
Hvis du ønsker å kunne arbeide med datoer i et mer avansert format, kan du vurdere å bruke en ekstern bibliotek for å håndtere dette i Arduino-koden din. Et populært bibliotek er [TimeLib](https://www.arduino.cc/reference/en/libraries/timelib/), som lar deg enkelt håndtere datoer og klokkeslett i forskjellige formater.
 
## Se også
 
- [Arduino sin offisielle dokumentasjon om String()-funksjonen](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [En guide til ulike måter å konvertere datoer til strenger i Arduino](https://www.robotshop.com/community/forum/t/how-do-i-convert-date-and-time-to-string/13095)