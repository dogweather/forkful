---
title:                "Elm: Å starte et nytt prosjekt"
programming_language: "Elm"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Hvorfor

Å starte et nytt programmeringsprosjekt kan virke skremmende for noen, spesielt hvis du ikke er kjent med Elm-programmeringsspråket. Men det kan også være en spennende mulighet til å utforske nye måter å bygge nettapplikasjoner på. Med sin funksjonelle og deklarative tilnærming, er Elm et flott verktøy for å skrive robust og feilfri kode. I tillegg er det et blomstrende samfunn av utviklere som er villige til å hjelpe og dele sine kunnskaper.

# Hvordan

For å starte et nytt prosjekt i Elm, må du først installere Elm-plattformen på datamaskinen din. Deretter kan du følge disse enkle trinnene:

1. Opprett en ny mappe for prosjektet ditt og naviger til den i terminalen din.
2. Skriv ```elm package install``` for å installere alle avhengigheter som er nødvendige for prosjektet.
3. Opprett en fil med navnet ```Main.elm``` og legg til følgende kode inne i den:

```
module Main exposing (..)

import Html

view : Html.Html
view =
  Html.text "Hei, verden!"

main =
  view
```

4. Kjør ```elm reactor``` i terminalen din for å starte en lokal server.
5. Åpne nettleseren din og gå til ```http://localhost:8000```. Du bør nå se teksten "Hei, verden!".

# Dypdykk

Når du har satt opp ditt første prosjekt i Elm, kan du begynne å utforske språket og dets funksjoner. Her er noen ressurser som kan være nyttige når du starter:

- Elm-dokumentasjonen: https://guide.elm-lang.org/
- Elm forum: https://discourse.elm-lang.org/
- Elm slack-kanal: https://elmlang.herokuapp.com/

Det er også mange gode Elm-baserte prosjekter og biblioteker tilgjengelig på GitHub som du kan bruke som referanse og læremateriell for å utvide din kunnskap om språket.

# Se også
- Elm offisiell nettside: https://elm-lang.org/
- Elm-sider på norsk: https://elm-lang.org/translate/no
- Elm subreddit: https://www.reddit.com/r/elm/