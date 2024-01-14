---
title:    "Haskell: Oppretting av en midlertidig fil"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger, når du koder, kommer du på et behov for å opprette midlertidige filer i programmet ditt. Dette kan være for å lagre midlertidige data, for testing, eller bare for å få en oversikt over hva som skjer i programmet. Uansett hva grunnen måtte være, kan Haskell gjøre dette enkelt for deg ved å bruke "System.IO" biblioteket.

## Slik gjør du det

For å opprette en midlertidig fil i Haskell, må du importere "System.IO" biblioteket og bruke funksjonen "openTempFile". Denne funksjonen tar to argumenter, en sti og et prefiks. Stien angir hvor filen skal opprettes, og prefikset vil bli lagt til det midlertidige filnavnet for å unngå navnekonflikter.

```Haskell
import System.IO

main = do
  (tmpFile, tmpHandle) <- openTempFile "." "temp"
  putStrLn $ "Midlertidig fil opprettet på sti " ++ tmpFile
```

I dette eksempelet, vil en midlertidig fil med navnet "temp[random tall]" bli opprettet i samme mappe som koden kjøres fra. Vi bruker "putStrLn" funksjonen for å skrive ut stien til filen, så vi kan se hvor den er plassert.

Etter at du har gjort de nødvendige endringene til den midlertidige filen, må du lukke håndteringsobjektet for filen med "hClose". Dette vil slette den midlertidige filen automatisk.

```Haskell
main = do
  (tmpFile, tmpHandle) <- openTempFile "." "temp"

  -- Etter å ha gjort endringer til den midlertidige filen
  hClose tmpHandle
```

## Dypdykk

Når du bruker "openTempFile" funksjonen, vil Haskell generere et unikt nummer for å legge til på slutten av filnavnet for å sikre at det er unikt og ikke vil krasje med andre filer. Dette nummeret genereres ved hjelp av "getRandom" funksjonen fra "System.Random" biblioteket.

En annen ting å merke seg er at hvis stien du oppgir ikke er gyldig, vil den midlertidige filen bli opprettet i det midlertidige meldomainnet "System.IO.tmpdir", som vanligvis er satt til "/tmp" på Unix-systemer.

## Se også

- ["System.IO" Hackage dokumentasjon](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- ["System.Random" Hackage dokumentasjon](https://hackage.haskell.org/package/random-1.2.1/docs/System-Random.html)