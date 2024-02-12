---
title:                "Controleren of een directory bestaat"
aliases:
- /nl/google-apps-script/checking-if-a-directory-exists/
date:                  2024-02-01T21:49:07.865728-07:00
model:                 gpt-4-0125-preview
simple_title:         "Controleren of een directory bestaat"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/google-apps-script/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Controleren of een map bestaat in Google Apps Script houdt in dat wordt geverifieerd of een map in Google Drive aanwezig is. Programmeurs voeren deze controle vaak uit om fouten of redundantie bij het aanmaken van mappen te voorkomen wanneer ze bestanden en mappen programmatisch beheren.

## Hoe doe je dat:

Google Apps Script biedt geen directe "bestaat" methode voor mappen. In plaats daarvan gebruiken we de zoekmogelijkheden van Google Drive om te controleren of een map met een specifieke naam bestaat. Hier is een stap-voor-stap voorbeeld:

```javascript
// Functie om te controleren of een map bestaat
function checkIfDirectoryExists(directoryName) {
  // Haal de verzameling mappen op die overeenkomen met de opgegeven naam
  var folders = DriveApp.getFoldersByName(directoryName);
  
  // Controleer of er minstens één map met de opgegeven naam bestaat
  if (folders.hasNext()) {
    Logger.log('Map bestaat.');
    return true;
  } else {
    Logger.log('Map bestaat niet.');
    return false;
  }
}

// Voorbeeldgebruik
var directoryName = 'Mijn Voorbeeldmap';
checkIfDirectoryExists(directoryName);
```

Voorbeelduitvoer:
```
Map bestaat.
```
of 
```
Map bestaat niet.
```

Dit script maakt gebruik van de methode `getFoldersByName`, die alle mappen in de Drive van de gebruiker ophaalt die overeenkomen met de opgegeven naam. Aangezien namen niet uniek zijn in Drive, retourneert deze methode een `FolderIterator`. De aanwezigheid van een volgend item (`hasNext()`) in deze iterator geeft aan dat de map bestaat.

## Diepgaande duik

Historisch gezien is bestandsbeheer in web- en cloudomgevingen aanzienlijk geëvolueerd. Google Apps Script, dat een uitgebreide API voor Google Drive biedt, maakt geavanceerde bestands- en mapbeheeroperaties mogelijk, inclusief de zoek- en controlemechanismen die worden gedemonstreerd. Een opmerkelijk aspect is echter het ontbreken van een directe controle op het bestaan, waarschijnlijk vanwege de toestemming van Google Drive voor meerdere mappen met dezelfde naam, wat contrasteert met veel bestandssystemen die unieke namen binnen dezelfde map afdwingen.

In deze context is het gebruiken van de methode `getFoldersByName` een effectieve oplossing, maar zou potentieel inefficiënties kunnen introduceren in een scenario waarin een groot aantal mappen met dubbele namen bestaat. Een alternatieve aanpak zou kunnen zijn het onderhouden van een applicatiespecifieke indexering of naamgevingsconventie om snellere controles te waarborgen, vooral wanneer prestaties een kritieke zorg worden.

Hoewel de aanpak van Google Apps Script in eerste instantie misschien minder direct lijkt in vergelijking met bestandscontroles in programmeertalen die direct met een enkel bestandssysteem zijn verbonden, weerspiegelt dit de noodzaak om de complexiteiten van cloudgebaseerde bestandsopslag te behandelen. Ontwikkelaars die Google Apps Script gebruiken voor Drive-beheer, moeten deze nuances overwegen, waarbij geoptimaliseerd wordt voor de sterke en beperkte punten van Google Drive.
