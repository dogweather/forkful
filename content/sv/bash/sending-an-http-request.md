---
title:                "Sända en http-begäran"
html_title:           "Bash: Sända en http-begäran"
simple_title:         "Sända en http-begäran"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad och Varför?

Att sända en HTTP förfrågan är en metod för att kommunicera med en webbserver och begära data. Programmerare använder denna metod för att hämta information från en viss URL eller för att interagera med en webbapplikation.

## Så här gör du:

För att sända en HTTP förfrågan i Bash kan du använda kommandot "curl". Här är ett enkelt exempel på hur du hämtar data från Google.com:

```Bash
curl google.com
```

Detta kommer att skriva ut HTML-koden för Google:s hemsida på din skärm. För att spara resultatet i en fil, kan du använda flaggan "-o" och ange en filnamn:

```Bash
curl -o google.html google.com
```

Detta kommer att spara HTML-koden i en fil som heter "google.html". Du kan också ange en specifik metod, såsom GET eller POST, och inkludera parametrar i din förfrågan. Se "Deep Dive"-sektionen för mer information om detta.

## Djupdykning:

Att sända HTTP förfrågningar i Bash är möjligt tack vare det kraftfulla verktyget "curl" som har funnits sedan 1997. Det finns dock flera alternativ för att sända HTTP förfrågningar, som till exempel genom att använda programmeringsspråk som Python eller JavaScript. Implementeringen av HTTP-protokollet möjliggör inte bara hämtning av data, utan också möjligheten att skicka och manipulera data, som till exempel vid inlämning av formulär på en webbsida.

## Se även:

Här är några användbara länkar för att lära dig mer om att sända HTTP förfrågningar i Bash:

- Officiell "curl" dokumentation: https://curl.se/docs/manpage.html
- En guide för att använda "curl" för att hämta data från en API: https://stackabuse.com/curl-the-command-line-tool-for-transferring-data/
- En fullständig förklaring av HTTP-protokollet: https://www.tutorialspoint.com/http/index.htm