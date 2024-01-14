---
title:    "Gleam: Å sjekke om en mappe eksisterer"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Hvorfor

Å sjekke om en mappe eksisterer er et viktig aspekt av programmering. Ved å sjekke om en mappe eksisterer, kan man sikre at ens program kjører som det skal og at den ønskede funksjonen blir utført. Det er også viktig for å unngå buggs og feil i koden.

# Hvordan

Gleam har en innebygd funksjon for å sjekke om en mappe eksisterer. Denne funksjonen heter `File.exists?` og tar inn en streng av navnet på mappen. La oss se på et eksempel:

```Gleam
let main() {
  let directory = File.exists?("bilder");
  IO.println("Eksisterer mappen 'bilder'?", directory);
}
```

I dette eksemplet sjekker vi om mappen "bilder" eksisterer. Hvis den gjør det, vil utskriften bli `true`, hvis ikke, vil vi få `false` som utskrift.

# Deep Dive

Når vi bruker `File.exists?` funksjonen i Gleam, er det viktig å merke seg at den vil returnere `true` selv om mappen ikke er tom. Dette betyr at selv om mappen kanskje ikke er tom, vil funksjonen likevel returnere `true` så lenge mappen eksisterer.

En annen ting å være oppmerksom på er at denne funksjonen også vil returnere `false` hvis den ikke har tilgang til mappen på grunn av sikkerhetsbegrensninger.

# Se også

- Dokumentasjon for `File.exists?`: https://gleam.run/modules/gleam/file.html#exists%3F
- Mer informasjon om mappebehandling i Gleam: https://gleam.run/guides/files.html