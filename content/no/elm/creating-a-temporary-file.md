---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Opprettelse av Midlertidige Filer i Elm

## Hva & Hvorfor?
Opprette midlertidig fil er handlingen av å lage en usalet fil for kort tid. Programmers bruker dette for lagring av mellomliggende data som ikke trenger å bli lagret for en lengre varighet.

## Hvordan gjøre: 
Elm har ingen innebygd mulighet til å håndtere filskriving eller -lesing på grunn av sin renhet og funksjonelle natur. Men du kan bruke porter for å kommunisere med JavaScript for å oppnå dette. Her er en eksempel på hvordan å gjøre det:

```Elm
port module Main exposing (..)

port sendToJS : String -> Cmd msg
```
Når sendToJS blir kalt, vil det utløse `sendToJS.subscribe` i din JavaScript kode:

```JavaScript
var app = Elm.Main.init();
app.ports.sendToJS.subscribe(function(tempFile) {
   // Handle temporary file creation with JS here
});
```
Legg merke til at mens dette lar deg opprette en midlertidig fil gjennom Elm, utføres den virkelige filopprettelsen i JavaScript, ikke Elm. 

## Dyp Dykk
Historisk sett har opprettelse av midlertidige filer vært utført i imperativ programmering. Når det gjelder funksjonelle programmeringsspråk som Elm, prøver de å unngå bieffekter, og å opprette filer er en slik bieffekt som skaper en varig forandring i systemet ditt.

Som alternativer, kan du vurdere å bruke server-side lagring eller in-memory databaser.

På detaljnivå, innebærer implementeringen å opprette en unik filnavn for å unngå kollisjoner, og deretter skrive data til denne filen. Dette kan gjøres ved å bruke program som `tempfile()` i JavaScript. 

## Se Også
- [Elm's offisielle om porter](https://guide.elm-lang.org/interop/ports.html)
- [StackOverflow diskusjon på filoperasjoner i Elm](https://stackoverflow.com/questions/43038198/how-to-read-write-a-local-file-in-elm)
- [Mozilla om FileReader API i JavaScript](https://developer.mozilla.org/en-US/docs/Web/API/FileReader)