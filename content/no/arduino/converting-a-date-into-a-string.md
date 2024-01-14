---
title:                "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du noen gang har hatt behov for å vise en dato i et Arduino-prosjekt, har du sikkert støtt på problemet med å konvertere datoen til en tekststreng. Dette er en vanlig utfordring for mange Arduino-entusiaster, og i denne bloggposten skal vi vise deg hvordan du kan løse dette problemet.

## Slik gjør du det

Det første du må gjøre er å starte med å definere en variabel for datoen. Vi vil bruke variabelnavnet "dato" i vårt eksempel, men du kan velge et annet navn etter eget ønske. Her er en kodebit som viser hvordan du kan definere datoen:

```Arduino
DateTime dato = DateTime(2021, 9, 14); // Dette er datoen du ønsker å konvertere
```

Neste steg er å konvertere datoen til en tekststreng. Dette kan gjøres ved å bruke funksjonen "to_string()", som er tilgjengelig for datatypen "DateTime". Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Arduino
Serial.println("Datoen er: " + dato.to_string());
```

Koden over vil skrive ut datoen som en tekststreng i serieporten. Avhengig av hvilken datoenhet du bruker, kan formatet på tekststrengen variere. Husk å sjekke dokumentasjonen for din enhet for å finne ut mer om gyldige formater.

## Dypdykk

Så hvordan fungerer egentlig konverteringen av datoen til en tekststreng? Når du kaller funksjonen "to_string()", bruker den datatypen "DateTime" innebygde funksjoner og metoder for å hente ut de ulike delene av datoen, som årstall, måned og dag. Deretter setter den sammen disse verdiene til en tekststreng i riktig format. Dette gjør konverteringsprosessen veldig enkel for deg som programmerer, siden du ikke trenger å tenke på de ulike delene av datoen og hvordan de skal formateres.

## Se også

- [DateTime dokumentasjon](https://www.arduino.cc/en/Reference/DateTime)
- [Serial.println() dokumentasjon](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/)
- [Intro to Arduino](https://www.makershield.com/til-alle-som-vil-l%C3%A6re-Men-det-er-mye-som-du-b%C3%B8r-vite.rar)_-En introduksjon til Arduino-programmering på norsk.