---
title:                "Lesing av en tekstfil"
html_title:           "TypeScript: Lesing av en tekstfil"
simple_title:         "Lesing av en tekstfil"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du er en programmerer som jobber med TypeScript, er det sannsynlig at du også må jobbe med å lese og behandle tekstfiler. Dette kan være nødvendig for å lagre eller hente data, eller rett og slett for å lese tekstfiler som en del av et større program. I denne artikkelen vil vi se på hvordan du kan lese en tekstfil ved hjelp av TypeScript.

## Hvordan

For å lese en tekstfil i TypeScript, må du først opprette en filreferanse ved hjelp av "fs" biblioteket. Deretter kan du bruke "readFile" funksjonen for å lese innholdet i filen. Her er et eksempel på hvordan du kan lese en tekstfil og skrive ut innholdet:

```typescript
import * as fs from "fs";

fs.readFile("tekstfil.txt", "utf-8", (error, data) => {
  if (error) throw error;
  console.log(data);
});
```

I dette eksempelet bruker vi "utf-8" som karakterkoding, men du kan velge en annen koding avhengig av behovene dine.

## Deep Dive

Det er verdt å merke seg at "fs" biblioteket ikke bare kan brukes til å lese tekstfiler, men også for å skrive, slette og endre filer. Du kan også lese innholdet i en fil som en binær buffer ved å ikke spesifisere noen karakterkoding.

En annen viktig ting å merke seg er håndtering av feil. I det første eksempelet bruker vi "throw" for å kaste en feil hvis det oppstår et problem ved lesingen av filen. Dette er bare en måte å håndtere feil på, og du kan velge å håndtere det på andre måter som passer dine preferanser.

## Se også

- [fs modulen dokumentasjon (på engelsk)](https://nodejs.org/api/fs.html)
- [TypeScript offisiell dokumentasjon (på engelsk)](https://www.typescriptlang.org/docs/)