---
title:                "Å jobbe med csv"
html_title:           "Arduino: Å jobbe med csv"
simple_title:         "Å jobbe med csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?

Arbeid med CSV står for Comma Separated Values og refererer til en måte å lagre og organisere data på ved hjelp av kommaer mellom verdier. Programmere bruker ofte CSV for å enkelt strukturere og lese data som er lagret i en tabellform.

# Hvordan:

En enkel måte å arbeide med CSV i Arduino er å bruke en CSV library som "ArduinoCSV". Først må du importere biblioteket ved å inkludere følgende linje i koden din:

```
#include <ArduinoCSV.h>
```

Deretter kan du definere et CSV objekt og åpne en fil for å lese data:

```
CSV csv;
csv.open("data.csv");
```

For å lese data fra filen rad for rad, kan du bruke en while-løkke og "readRow" kommandoen:

```
while (csv.readRow()) {
  int value1 = csv.getInt(0);
  float value2 = csv.getFloat(1);
  String value3 = csv.getString(2);
  // gjør noe med dataen her
}
```

CSV objektet vil automatisk lese hvert komma separert felt og lagre det som en int, float eller string, avhengig av verdien. Du kan også bruke kommandoer som "writeRow" for å skrive data til filen.

# Dypdykk:

CSV formatet ble opprinnelig utviklet på 1970-tallet som en standard for å overføre data fra en database til en annen. Det har blitt svært populært og brukes ofte i applikasjoner som håndterer store mengder data.

Det finnes også andre måter å lagre data på, som for eksempel XML og JSON formatene. Disse kan være mer komplekse å arbeide med, men tilbyr mer struktur og fleksibilitet.

Implementasjonen av CSV i Arduino er avhengig av biblioteker, da det ikke finnes en innebygd funksjonalitet for dette. Det er viktig å være oppmerksom på eventuelle begrensninger eller utfordringer med de valgte bibliotekene.

# Se også:

- ArduinoCSV bibliotek: https://github.com/arduino-libraries/ArduinoCSV
- Wikipedia: https://en.wikipedia.org/wiki/Comma-separated_values
- How to CSV in Arduino: https://www.youtube.com/watch?v=VPrUeIH4rxI