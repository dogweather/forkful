---
title:                "Sjekke om en katalog eksisterer"
aliases:
- no/google-apps-script/checking-if-a-directory-exists.md
date:                  2024-02-01T21:49:02.481398-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sjekke om en katalog eksisterer"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/google-apps-script/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sjekke om en mappe eksisterer i Google Apps Script innebærer å bekrefte tilstedeværelsen av en mappe inne i Google Drive. Programmerere utfører ofte dette sjekket for å unngå feil eller unødvendig oppretting av mapper når de håndterer filer og kataloger programmert.

## Hvordan gjøre det:

Google Apps Script tilbyr ikke en direkte "eksisterer" metode for mapper. I stedet bruker vi Google Drives søkefunksjoner for å sjekke om en mappe med et spesifikt navn eksisterer. Her er et trinn-for-trinn eksempel:

```javascript
// Funksjon for å sjekke om en mappe eksisterer
function checkIfDirectoryExists(directoryName) {
  // Hent samlingen av mapper som matcher det spesifiserte navnet
  var folders = DriveApp.getFoldersByName(directoryName);
  
  // Sjekk om det finnes minst én mappe med det spesifiserte navnet
  if (folders.hasNext()) {
    Logger.log('Mappen eksisterer.');
    return true;
  } else {
    Logger.log('Mappen eksisterer ikke.');
    return false;
  }
}

// Eksempel på bruk
var directoryName = 'Min Eksempelmappe';
checkIfDirectoryExists(directoryName);
```

Eksempel på utskrift:
```
Mappen eksisterer.
```
eller 
```
Mappen eksisterer ikke.
```

Dette skriptet benytter seg av metoden `getFoldersByName` som henter alle mapper i brukerens Drive som samsvarer med det spesifiserte navnet. Siden navn ikke er unike i Drive, returnerer denne metoden en `FolderIterator`. Tilstedeværelsen av et neste element (`hasNext()`) i denne iteratoren indikerer at mappen eksisterer.

## Dypdykk

Historisk sett har filhåndtering i nett- og skymiljøer utviklet seg betydelig. Google Apps Script, som tilbyr et omfattende API for Google Drive, tillater avanserte operasjoner for håndtering av filer og mapper, inkludert søke- og sjekkmekanismene som er demonstrert. Imidlertid er et bemerkelsesverdig aspekt mangelen på en direkte eksistenssjekk, sannsynligvis på grunn av Google Drives tillatelse for flere mapper med samme navn, noe som er i kontrast til mange filsystemer som håndhever unike navn innenfor samme katalog.

I denne konteksten er bruk av metoden `getFoldersByName` en effektiv omvei, men kan potensielt introdusere ineffektiviteter i et scenario hvor det eksisterer et stort antall mapper med duplikatnavn. Et alternativt tilnærming kunne innebære å opprettholde en applikasjonsspesifikk indeksering eller navngivningskonvensjon for å sikre raskere sjekker, spesielt når ytelsen blir en kritisk bekymring.

Selv om Google Apps Scripts tilnærming kan virke mindre direkte sammenlignet med filtilstedeværelseskontroller i programmeringsspråk som direkte er grensesnittet mot et enkelt filsystem, reflekterer det nødvendigheten av å håndtere kompleksiteten til skylagring av filer. Utviklere som utnytter Google Apps Script for Drive-håndtering bør vurdere disse nyansene, og optimalisere for Google Drives styrker og begrensninger.
