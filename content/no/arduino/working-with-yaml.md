---
title:                "Arduino: Jobbe med yaml"
simple_title:         "Jobbe med yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du prøvd å programmere med Arduino, men slitt med å håndtere store datamengder? Da kan YAML være løsningen for deg! Med YAML kan du enkelt lagre og håndtere data i et leservennlig format. Les videre for å lære mer om hvordan du kan bruke YAML i dine Arduino prosjekter.

## Hvordan
For å komme i gang med YAML i Arduino, må du først installere biblioteket "ArduinoYaml". Dette kan gjøres ved å åpne Arduino IDE, gå til "Verktøy" og deretter "Administrer biblioteker". Søk etter "Yaml" og velg "ArduinoYaml" fra listen. Trykk på "Installer" for å legge til biblioteket i din Arduino IDE.

Etter å ha installert biblioteket, er det på tide å begynne å kode! Først må du importere biblioteket ved å legge til ```#include <Yaml.h>``` på toppen av koden din.

La oss lage et enkelt eksempel hvor vi lagrer informasjon om en person i YAML-format. Vi vil lagre navn, alder og favorittfarge. Kodeeksempelet vil se slik ut:

```
#include <Yaml.h>

void setup() {
  Serial.begin(9600);
  
  YamlObject person; // Oppretter et YamlObject med navnet "person"
  person["Navn"] = "Ole Olsen"; // Legger til en nøkkel "Navn" og en verdi "Ole Olsen"
  person["Alder"] = 35; // Legger til en nøkkel "Alder" og en verdi 35
  person["Favorittfarge"] = "Blå"; // Legger til en nøkkel "Favorittfarge" og en verdi "Blå"
  
  Serial.println(person); // Skriver ut person-objektet
}

void loop() {
  
}

```

Når vi kjører koden og åpner Serial Monitor, vil vi få følgende output:

```
Navn: Ole Olsen
Alder: 35
Favorittfarge: Blå
```

Vi kan også lagre YAML-data i en fil ved hjelp av funksjonen "saveFile()":

```
person.saveFile("person.yml"); // Lager en fil kalt "person.yml" og lagrer dataen her
```

## Dykk Dypere
Det er mange flere funksjoner og muligheter med ArduinoYaml biblioteket. For å lære mer, anbefales det å se på dokumentasjonen som følger med biblioteket, samt å eksperimentere og prøve ut forskjellige funksjoner.

## Se Også
- [ArduinoYaml Biblioteket](https://github.com/arduino/ArduinoYaml)
- [ArduinoYaml Dokumentasjon](https://arduino.github.io/ArduinoYaml/index.html)