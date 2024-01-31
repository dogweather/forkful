---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä tekstintiedoston kirjoittaminen on? Se on tallennusprosessi, jossa data muutetaan tekstimuotoon ja kirjoitetaan tiedostoon. Koodaajat kirjoittavat tekstitiedostoja datan tallentamiseksi, siirtämiseksi tai varmuuskopioimiseksi.

## How to:
Elmillä ei voi suoraan kirjoittaa tiedostoja, koska se suunniteltu turvalliseksi kielenä, joka toimii selaimissa. Sen sijaan käytetään JavaScriptiä Elm:n kanssa viestintään. 

Tässä kuinka voit esimerkiksi luoda "Tallenna tiedosto" -toiminnon käyttäen Elm:n `Ports`-ominaisuutta:

```Elm
port module Main exposing (..)

-- Määrittele portti tiedoston lataamiseen
port download : String -> Cmd msg

-- Käynnistä lataus painikkeesta
saveToFile : String -> Cmd msg
saveToFile data =
    download data
```

Ja JavaScriptissä:

```javascript
app.ports.download.subscribe(function(data) {
    var blob = new Blob([data], {type: 'text/plain'});
    var fileUrl = URL.createObjectURL(blob);
    var tempLink = document.createElement('a');
    tempLink.href = fileUrl;
    tempLink.setAttribute('download', 'filename.txt');
    tempLink.click();
});
```

## Deep Dive
Elm on julkaistu vuonna 2012, ja sen suunnittelun ytimessä on turvallisuus ja yksinkertaisuus. Suoraa tiedostojen käsittelyä ei ole, koska se voisi lisätä tietoturvariskejä. Vaihtoehtoina tiedon tallennukseen ovat Local Storage tai ulkoiset palvelut/apit, joita voi kutsua käyttäen `Http`-moduuleita. `Ports` tarjoaa joustavan tavan integroida Elm ja JavaScript tarvittaessa.

## See Also
- Elm Official Documentation: Ports - https://guide.elm-lang.org/interop/ports.html
- MDN Web Docs: Blob - https://developer.mozilla.org/en-US/docs/Web/API/Blob
- Elm Discourse: Sharing Experiences and Patterns - https://discourse.elm-lang.org/
