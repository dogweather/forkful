---
title:                "Att skicka en http-begäran"
html_title:           "Go: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

En HTTP-förfrågning är en signal som skickas till en server för att hämta eller skicka information över Internet. Programmerare gör detta för att manipulera data, till exempel när en webbplats behöver visa innehåll från en databas.

## Hur man gör:

För att skicka en HTTP-förfrågan i Gleam, kan du använda `gleam/httpc` biblioteket. Här är ett exempel:

```gleam
import gleam/httpc
import gleam/http.{Get}

pub fn skicka_förfrågan() -> Result(BitString, httpc.Error) {
    let request = Get("https://exempelplats.se")
    httpc.send(request)
}
```
Detta kommer att skicka en GET-förfrågan till "https://exempelplats.se" och ger ett svar som antingen en sträng med hjälp av `Result(BitString, httpc.Error)` eller ett felmeddelande.

## Fördjupning:

Historiskt sett behövde programmerare manuellt hantera TCP / IP-protokoll för att skicka HTTP-förfrågningar. Men nu erbjuder moderna programmeringsspråk som Gleam inbyggda bibliotek för det.

Alternativ till `gleam/httpc` inkluderar `lhttpc` och `hackney`, men dessa tillhandahålls inte direkt av Gleam och kan behöva ytterligare konfiguration.

När det gäller implementeringsdetaljer används `gleam/httpc`-biblioteket för att sköta HTTP/2 protokollet. Det gör att Gleam-programmet kan kommunicera effektivt med webbservern.

## Se Även:

För mer detaljer om `gleam/httpc`, besök dess dokumentation:<br>
[httpc](https://hexdocs.pm/gleam_stdlib/httpc.html)
  
Om du är intresserad av att lära dig mer om HTTP / 2 protokollet, kolla in denna länk:<br>
[HTTP / 2](https://developers.google.com/web/fundamentals/performance/http2)

Läs mer om historien om HTTP-förfrågningar här:<br>
[Historien om HTTP-förfrågningar](https://www.tutorialspoint.com/http/http_quick_guide.htm) 

För mer information om andra bibliotek för att skicka HTTP-förfrågningar i Gleam, se:<br>
[lhttpc](https://github.com/esl/lhttpc)<br>
[hackney](https://github.com/benoitc/hackney)