---
date: 2024-01-20 17:52:35.831459-07:00
description: "Skriva ut fels\xF6kningsdata g\xF6r det l\xE4ttare att se vad ditt program\
  \ egentligen g\xF6r. Programmerare anv\xE4nder det f\xF6r att hitta buggar och f\xF6\
  rst\xE5 programfl\xF6det."
lastmod: '2024-03-13T22:44:37.831958-06:00'
model: gpt-4-1106-preview
summary: "Skriva ut fels\xF6kningsdata g\xF6r det l\xE4ttare att se vad ditt program\
  \ egentligen g\xF6r. Programmerare anv\xE4nder det f\xF6r att hitta buggar och f\xF6\
  rst\xE5 programfl\xF6det."
title: "Skriva ut fels\xF6kningsdata"
---

{{< edit_this_page >}}

## What & Why? - Vad & Varför?
Skriva ut felsökningsdata gör det lättare att se vad ditt program egentligen gör. Programmerare använder det för att hitta buggar och förstå programflödet.

## How to: - Hur gör man:
I Elm använder vi `Debug.log` för att skriva ut värden i konsolen:

```Elm
import Html

main =
  Html.text (Debug.log "MyDebugValue" "Hello, Elm!")
```

Sample output in the browser console would look like:

```
MyDebugValue: "Hello, Elm!"
```

Observera att `Debug.log` tar två argument: en etikett (string) och värdet du vill skriva ut. Det returnerar värdet som det är så att du kan koda som vanligt.

## Deep Dive - Fördjupning:
`Debug.log` är enkel men följer Elm's filosofi om renhet och transparens i kodning. Det introducerades i de tidiga versionerna av Elm och finns kvar eftersom det är ett ovärderligt verktyg. Alternativt kan du använda `Debug.todo` för att markera ofärdiga delar i koden. Värdet av att kunna skriva ut felsökningsdata utan att störa programmets flöde kan inte underskattas, men kom ihåg att ta bort `Debug.log` uttryck innan du bygger din slutgiltiga version.

## See Also - Se även:
- Elm's officiella debug-dokumentation: [Elm Debugging](https://package.elm-lang.org/packages/elm/browser/latest/Browser#debugging)
- Diskussion om felsökning i Elm på Elm Discourse: [Elm Discourse](https://discourse.elm-lang.org/c/learn)
- Bloggpost om felsökningserfarenheter: [Elm Debugging Experience](https://elm-lang.org/news/the-perfect-bug-report)
