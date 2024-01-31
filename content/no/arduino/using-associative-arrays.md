---
title:                "Bruke associative tabeller"
date:                  2024-01-30T19:10:11.601710-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke associative tabeller"

tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva og hvorfor?
I Arduino-verdenen lar assoiative matriser deg pare nøkler med verdier, litt som å matche sokker med deres par. De er et førstevalg når du trenger å lagre og hente data ved hjelp av beskrivende navn, noe som gjør koden din renere og mye mer forståelig.

## Hvordan:
Arduino, strengt tatt, har ikke innebygd støtte for assoiative matriser slik du ville funnet i høyere programmeringsspråk. Men, frykt ikke. Vi kan være kreative ved å bruke strukturer og matriser for å etterligne denne funksjonaliteten. Her er et enkelt eksempel på å lage en grunnleggende "assosiativ matrise" for lagring og tilgang til temperaturer for forskjellige byer.

Først, definer en struktur for å holde byen (nøkkel) og dens temperatur (verdi):

```cpp
struct CityTemperature {
  String city;
  float temperature;
};
```

Deretter, initialiser en matrise av `CityTemperature` objekter:

```cpp
CityTemperature temperatures[] = {
  {"New York", 19.5},
  {"Los Angeles", 22.0},
  {"Chicago", 17.0}
};
```

Slik kan du få tilgang til og vise temperaturen for en spesifikk by:

```cpp
void setup() {
  Serial.begin(9600);
  for(int i = 0; i < 3; i++) {
    if(temperatures[i].city == "Los Angeles") {
      Serial.print("Temperaturen i Los Angeles er: ");
      Serial.println(temperatures[i].temperature);
    }
  }
}

void loop() {
  // Ingenting her for nå.
}
```

Å kjøre denne koden vil gi deg utskriften:

```
Temperaturen i Los Angeles er: 22.0
```

## Dypdykk
Historisk sett kom programmeringsspråk som C og C++ (som Arduino-syntaksen er avledet fra) ikke med innebygde assoiative matriser, noe som førte til løsninger som den vist ovenfor. Denne tilnærmingen er relativt enkel, men skalerer dårlig etter hvert som datamengden øker på grunn av dens O(n) opptid.

Språk som Python tilbyr ordbøker, og JavaScript har objekter for dette formålet, begge er langt mer effektive for å håndtere nøkkel-verdipar. I Arduino, når ytelse og effektivitet blir kritisk, kan utviklere velge mer spesialiserte datatstrukturer, som hashtabeller, implementert via biblioteker.

Selv om Arduino ikke støtter assoiative matriser nativt, har fellesskapet utviklet biblioteker som `HashMap` som kan legges til prosjektet ditt for å tilby lignende funksjonalitet med bedre ytelse enn en gjør-det-selv-tilnærming. Disse bibliotekene tilbyr vanligvis en mer elegant og effektiv måte å håndtere assoiative matriser på, spesielt for mer komplekse prosjekter.
