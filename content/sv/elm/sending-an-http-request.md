---
title:                "Skicka en http-begäran"
html_title:           "Elm: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/sending-an-http-request.md"
---

{{< edit_this_page >}}

Vad & Varför?

Att skicka en HTTP-förfrågan är när en dator begär information från en annan dator via internet. Programmerare använder detta för att få tillgång till data som till exempel webbsidor eller API:er.

Hur man gör:

Elm har ett inbyggt HTTP-paket som gör det enkelt att skicka förfrågningar. Nedan finns ett exempel på en GET-förfrågan till en API som returnerar en lista med användare i JSON-format.

```Elm http.get "https://example.com/users"``` 

Några av de möjliga svar man kan få från detta API inkluderar en 200 statuskod för en lyckad förfrågan och en JSON-lista med användare i svaret:

```Elm
{ status = 200
  body = "[{ \"name\": \"Alice\", \"age\": \"25\" }, { \"name\": \"Bob\", \"age\": \"30\" }, { \"name\": \"Charlie\", \"age\": \"40\" }]"
}
```

Djupdykning:

Skicka HTTP-förfrågningar har varit en viktig del av webbutveckling sedan dess början. Innan Elm fanns det många andra alternativ för att skicka förfrågningar, såsom jQuery eller XMLHttpRequest. Elm's HTTP-paket förenklar detta processen genom att hantera alla aspekter av HTTP-kommunikation, inklusive felhantering och parsning av JSON-svar.

Se även:

Läs mer om Elm's HTTP-paket i dokumentationen: https://package.elm-lang.org/packages/elm/http/latest/

Lär dig mer om HTTP-protokollets historia och hur det används: https://developer.mozilla.org/en-US/docs/Web/HTTP

Utforska andra alternativ för att skicka HTTP-förfrågningar, såsom Fetch API eller Axios: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API https://github.com/axios/axios