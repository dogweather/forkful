---
title:                "C++: Arbeide med yaml"
simple_title:         "Arbeide med yaml"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/cpp/working-with-yaml.md"
---

{{< edit_this_page >}}

## Hvorfor

YAML (YAML Ain't Markup Language) er et populært og enkelt format for å lagre og overføre strukturerte data i programmering. Det brukes ofte for konfigurasjonsfiler og dataoverføring mellom forskjellige systemer. Ved å lære hvordan man jobber med YAML, kan du effektivt håndtere data i ulike formater og integrere dem i dine programmer.

## Slik gjør du det

For å arbeide med YAML i C ++, må du først laste ned et YAML-bibliotek som gir funksjoner for å lese og skrive YAML-filer. Et populært bibliotek er YAML-cpp, som kan installeres via pakkesjefen din eller lastes ned fra GitHub.

Etter at du har installert biblioteket, kan du begynne å bruke det i dine programmer. Her er et eksempel på hvordan du kan lese og skrive data fra en YAML-fil ved hjelp av YAML-cpp:

```C++
#include <yaml-cpp/yaml.h>
#include <iostream>

int main() {
  // Leser data fra YAML-fil
  YAML::Node data = YAML::LoadFile("eksempel.yml");

  // Henter informasjon fra YAML-filen
  std::string navn = data["navn"].as<std::string>();
  int alder = data["alder"].as<int>();

  // Skriver ut informasjon
  std::cout << "Navn: " << navn << std::endl;
  std::cout << "Alder: " << alder << std::endl;

  // Oppdaterer alderen og lagrer til YAML-filen
  data["alder"] = 28;
  YAML::Emitter out;
  out << data;
  std::ofstream file("eksempel.yml");
  file << out.c_str();
}
```

I dette eksempelet bruker vi YAML::LoadFile-funksjonen for å lese data fra en YAML-fil og lagrer dataene i en YAML::Node-variabel. Deretter henter vi ut informasjonen vi trenger fra noden ved hjelp av "navn" og "alder"-nøklene. Til slutt oppdaterer vi alderen og lagrer de endrede dataene tilbake i filen ved hjelp av YAML::Emitter og std::ofstream.

## Dypdykk

YAML-cpp tilbyr flere funksjoner for å håndtere komplekse datastrukturer, inkludert støtte for lister, kart og innebygde typer som strenger og tall. Du kan også bruke operatorer som << og >> for å enkelt lese og skrive data til YAML-filer.

I tillegg til å lese og skrive data, tilbyr YAML-cpp også muligheten til å validere og konvertere YAML-data til andre formater som JSON og XML.

Sjekk ut dokumentasjonen til YAML-cpp for å få full oversikt over biblioteket og dets funksjoner.

## Se også

- YAML-cpp dokumentasjon: https://github.com/jbeder/yaml-cpp/wiki
- YAML-spesifikasjon: https://yaml.org/spec/1.2/spec.html