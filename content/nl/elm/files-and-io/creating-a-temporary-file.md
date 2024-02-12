---
title:                "Een tijdelijk bestand aanmaken"
date:                  2024-01-28T21:58:27.097759-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een tijdelijk bestand aanmaken"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elm/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

# Wat & Waarom?

Het creëren van een tijdelijk bestand betekent het maken van een bestand dat is ontworpen voor kortstondig gebruik. Programmeurs doen dit om redenen zoals het beschermen van gevoelige gegevens of het beheren van tussentijdse resultaten tijdens een proces.

# Hoe te:

Elm draait in browsers en heeft daardoor geen directe toegang tot het bestandssysteem. Daarom kun je geen traditionele tijdelijke bestanden maken. Maar, als je een soortgelijke functie nodig hebt, gebruiken we Elm-poorten (ports) om te communiceren met JavaScript, dat wel tijdelijke bestanden kan aanmaken.

```elm
port module Main exposing (..)

-- Definieer een port voor het creëren van een tijdelijk bestand in JavaScript
port createTempFile : String -> Cmd msg

-- Stuur gegevens naar JavaScript om een tijdelijk bestand te maken
saveDataTemporarily : String -> Cmd msg
saveDataTemporarily data =
    createTempFile data
```

Voor het JavaScript-deel, met gebruik van de File API:

```javascript
app.ports.createTempFile.subscribe(function(data) {
    var blob = new Blob([data], {type: 'text/plain'});
    var url = URL.createObjectURL(blob);

    // Hier kun je de URL gebruiken om de blob te downloaden of door te geven aan andere delen van je app
    console.log(url);  // Het logt de URL van het tijdelijke bestand
});
```

Voorbeelduitvoer in JavaScript-console:

```plaintext
blob:null/2135a9b7-1aad-4e7a-8bce-19c4f3f6d7ff
```

# Diepere Duik

Elm is ontworpen om veilig en betrouwbaar te zijn, dus directe toegang tot het bestandssysteem staat niet op het programma. In plaats daarvan gebruikt Elm poorten (ports) om te communiceren met JavaScript, wat het mogelijk maakt om operaties zoals het maken van tijdelijke bestanden uit te voeren. Historisch gezien handelen we bestandsgebaseerde taken in de browser af via JavaScript-API's, waarbij we Elm gebruiken voor typeveilige, high-level logica.

Alternatieven zoals WebAssembly kunnen in de toekomst mogelijk meer directe bestandssysteeminteracties toestaan, maar voor nu is interoperabiliteit met JavaScript de standaardpraktijk.

Wat implementatie betreft, betekent het creëren van tijdelijke bestanden in de browsercontext geen daadwerkelijk bestand op het bestandssysteem, maar eerder een in het geheugen opgeslagen representatie (blob) waarmee je kunt werken en die je indien nodig kunt opslaan.

# Zie Ook

- [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
- [MDN - Web-API's - Bestand](https://developer.mozilla.org/en-US/docs/Web/API/File)
- [MDN - Web-API's - Blob](https://developer.mozilla.org/en-US/docs/Web/API/Blob)
