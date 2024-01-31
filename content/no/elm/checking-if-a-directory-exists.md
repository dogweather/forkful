---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:56:26.611961-07:00
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"

category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekke om en mappe finnes innebærer å bekrefte at en bestemt sti leder til en eksisterende katalog. Programmere gjør dette for å unngå feil før de forsøker operasjoner som lese fra eller skrive til filsystemet.

## Hvordan gjøre det:
Elm kjører i nettleseren og har ikke direkte tilgang til filsystemet. Du må derfor bruke `ports` for å kommunisere med JavaScript, som kan sjekke filsystemet.

```Elm
port module Main exposing (..)

-- Definerer en port for å sende en sti til JavaScript
port checkDirectory : String -> Cmd msg

-- Definerer en port for å få svaret
port directoryExists : (Bool -> msg) -> Sub msg
```

I JavaScript ville du hatt noe lignende:

```JavaScript
// Definerer en funksjon for å sjekke om mappen finnes
function doesDirectoryExist(path) {
  const fs = require('fs');
  try {
    return fs.existsSync(path) && fs.lstatSync(path).isDirectory();
  } catch (err) {
    return false;
  }
}

// Abonnerer på Elm-porten
app.ports.checkDirectory.subscribe(function(path) {
  app.ports.directoryExists.send(doesDirectoryExist(path));
});
```

Elm vil sende en sti ned til JavaScript, som da sjekker om mappen finnes og sender tilbake et `Bool`.

## Dybde Undervannsbåt
Elm er designet for webapplikasjoner, og har ingen innebygd evne til å håndtere filsystemet direkte ettersom det kjører i nettleseren. Historisk sett har dette betydd at Elm-applikasjoner må stole på server-side logikk eller JavaScript interop via `ports` for filsystemoppgaver.

Alternativer for en full backend-løsning inkluderer å benytte elm-serverless, som lar deg kjøre Elm på servere, eller å skrive en Node.js-tjeneste i JavaScript eller TypeScript som din Elm frontend kan kommunisere med gjennom HTTP.

Når det gjelder implementeringsdetaljer, er sjekk av om en mappe finnes relativt rett frem i JavaScript ved bruk av `fs`-modulen, som i eksempelet over. Elm-porter formidler data mellom din Elm-kode og potensialet for sideeffekter i JavaScript.

## Se Også
- Elm ports dokumentasjon: https://guide.elm-lang.org/interop/ports.html
- Node.js `fs` modul dokumentasjon: https://nodejs.org/api/fs.html
- Elm-serverless prosjektet: https://github.com/ktonon/elm-serverless
