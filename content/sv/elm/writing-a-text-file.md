---
title:                "Skriva en textfil"
date:                  2024-01-19
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva en textfil innebär att spara data i en fil på datorn. Det gör programmerare för att spara tillstånd, konfigurationer eller dela data mellan system.

## Hur gör man:
I Elm kan du inte direkt skriva en textfil på grund av dess natur som ett språk för webbapplikationer. Men, du kan skapa en nedladdningslänk för att spara innehåll till en fil:

```Elm
import Html
import Html.Attributes
import Url

downloadCsv : Html.Html msg
downloadCsv =
    let
        csvContent = "name,age\nAlice,42\nBob,27"
        encodedContent = Url.percentEncode csvContent
        dataUrl = "data:text/csv;charset=utf-8," ++ encodedContent
    in
    Html.a [ Html.Attributes.href dataUrl, Html.Attributes.download "info.csv" ] [ Html.text "Ladda ner som CSV" ]

main = downloadCsv
```

## Djupdykning:
Historiskt sett har Elm-fokuset legat på säkerhet och moteckning av buggar snarare än direkt filhantering, vilket sker på serversidan eller med hjälp av JavaScript-interoperability (`ports` i Elm). Alternativ som Web APIs File och FileReader kan användas via `ports`. För fullständig filskrivning på klienten kan man behöva ett mer traditionellt språk som JavaScript.

## Se även:
- Elm Ports dokumentation: https://guide.elm-lang.org/interop/ports.html
- Elm-webbplats: https://elm-lang.org/
- MDN Web Docs – File API: https://developer.mozilla.org/en-US/docs/Web/API/File
