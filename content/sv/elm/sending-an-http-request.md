---
title:                "Elm: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka HTTP-förfrågningar är viktigt i dagens webbutveckling. Det gör det möjligt för våra applikationer att kommunicera med andra serverar och hämta data i realtid. I Elm kan vi skicka HTTP-förfrågningar på ett enkelt och säkert sätt, vilket gör det till en viktig färdighet för alla som vill bygga kraftfulla och effektiva webbutvecklingsapplikationer.

## Så här gör du

För att skicka en HTTP-förfrågan i Elm, behöver du först importera paketet "Http". Sedan kan du använda funktionen "send" för att specificera typen av förfrågan (GET, POST, etc.), URL:en och eventuell data som behövs för förfrågan.

```
Elm.Http.send (
    Elm.Http.request "GET" "http://example.com/api" (Elm.Http.jsonBody { 
        key = "value"
    }
)
```

Detta kodexempel skickar en GET-förfrågan till URL:en "http://example.com/api" med dataobjektet `{ key = "value" }` som en JSON-kropp.

## Deep Dive

När vi skickar en HTTP-förfrågan i Elm, så använder vi den integrerade Elm-HTTP-klienten. Denna klient hanterar allt från att bygga korrekta förfrågningar till att hantera eventuella felmeddelanden. Det gör att vi som utvecklare kan fokusera på att bygga funktionella applikationer utan att behöva oroa oss för detaljerna i HTTP-kommunikationen.

För mer information om hur du skickar HTTP-förfrågningar och hur du hanterar svar i Elm, kan du besöka Elm:s officiella dokumentation om HTTP-paketet.

## Se också

- Elm:s dokumentation om HTTP-paketet: https://package.elm-lang.org/packages/elm/http/latest/
- "HTTP i Elm" av Richard Feldman: https://www.youtube.com/watch?v=x9YDQUJwfuA&feature=youtu.be
- Översikt över webbutveckling med Elm: https://guide.elm-lang.org/webapps/