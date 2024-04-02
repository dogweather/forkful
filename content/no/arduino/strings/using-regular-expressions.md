---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:55.440404-07:00
description: "Regul\xE6re uttrykk (regex) er sekvenser av tegn som definerer s\xF8\
  kem\xF8nstre, prim\xE6rt brukt for strengsammenligning og manipulering. Programmerere\
  \ utnytter\u2026"
lastmod: '2024-03-13T22:44:41.048742-06:00'
model: gpt-4-0125-preview
summary: "Regul\xE6re uttrykk (regex) er sekvenser av tegn som definerer s\xF8kem\xF8\
  nstre, prim\xE6rt brukt for strengsammenligning og manipulering. Programmerere utnytter\u2026"
title: "Bruke regul\xE6re uttrykk"
weight: 11
---

## Hva & Hvorfor?
Regulære uttrykk (regex) er sekvenser av tegn som definerer søkemønstre, primært brukt for strengsammenligning og manipulering. Programmerere utnytter regex i Arduino-prosjekter for å analysere serielle innganger, validere brukerinndata, eller ekstrahere data fra strenger, noe som øker effektiviteten og fleksibiliteten i databehandlingen.

## Hvordan:
Arduino har ikke innebygd støtte for regex direkte i sitt standardbibliotek. Imidlertid kan du oppnå regex-lignende funksjonalitet for enkle mønstre ved å bruke grunnleggende strengfunksjoner, eller for mer komplekse behov, integrere et tredjeparts bibliotek som `regex`.

### Grunnleggende Strengsammenligning uten Regex
For grunnleggende behov, som å finne en delstreng, kan du bruke `String.indexOf()`-funksjonen:
```cpp
String data = "Sensorverdi: 12345";
int index = data.indexOf("verdi:");
if (index != -1) {
  String verdi = data.substring(index + 6).trim();
  Serial.println(verdi); // Utganger: 12345
}
```

### Bruk av Et Tredjeparts Bibliotek for Regex
For å håndtere mer komplekse mønstre, kan du vurdere et bibliotek som `regex`. Etter å ha installert biblioteket, kan du bruke det som følger:

1. **Installasjon**: `regex`-biblioteket er kanskje ikke umiddelbart tilgjengelig i Arduino Library Manager, så du kan trenge å manuelt installere det ved å laste det ned fra en pålitelig kilde og legge det til i Arduino-bibliotekene dine.

2. **Eksempel På Bruk**:
Forutsatt at biblioteket gir funksjonalitet lik standard regex-implementeringer, kan du bruke det som følger:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // Vent på at Serial skal være klar
  
  regex_t reg;
  const char* mønster = "[0-9]+"; // Matcher en sekvens av sifre
  regcomp(&reg, mønster, REG_EXTENDED);
  
  const char* test_str = "Sensorverdi: 12345";
  
  regmatch_t matcher[1];
  if (regexec(&reg, test_str, 1, matcher, 0) == 0) {
    // Ekstraher og skriv ut den matchende delen
    int start = matcher[0].rm_so;
    int slutt = matcher[0].rm_eo;
    char match[slutt-start+1];
    strncpy(match, test_str + start, slutt-start);
    match[slutt-start] = '\0';
    
    Serial.print("Fant match: ");
    Serial.println(match); // Utganger: 12345
  } else {
    Serial.println("Ingen match funnet");
  }
  
  regfree(&reg); // Frigjør den allokerte minne for regex
}

void loop() {
  // sett din hovedkode her, for å kjøre gjentatte ganger:
}
```

**Merk**: Syntaksen og de spesifikke funksjonene brukt her er til illustrative formål og kan variere basert på de faktiske implementasjonsdetaljene til `regex`-biblioteket du velger. Referer alltid til bibliotekets dokumentasjon for nøyaktig og oppdatert informasjon.
