---
title:                "Skicka en http-begäran"
html_title:           "Fish Shell: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka HTTP-förfrågningar är nödvändigt för att kommunicera med servrar och hämta data från internet. Genom att använda Fish Shell kan du enkelt automatisera detta och integrera det med andra kommandon och skript.

## Hur du gör det

Skicka en HTTP-förfrågan med Fish Shell är enkelt. Först behöver du installera cURL, ett vanligt verktyg för att utföra HTTP-kommunikationer. Detta kan du göra genom att köra följande kommando i terminalen:

```Fish Shell
sudo apt-get install curl
```

När cURL är installerat kan du skicka en GET-förfrågan med hjälp av följande kommando:

```Fish Shell
curl www.example.com
```

Detta kommer att skicka en förfrågan till www.example.com och returnera resultatet i terminalen. Du kan också lägga till olika flaggor för att anpassa din förfrågan, såsom att specificera ett annat HTTP-metod, ange headers eller skicka data.

## Djupdykning

Fish Shell har också stöd för att skicka HTTP-förfrågningar med API:et "HTTPie". Detta är ett alternativ till cURL som erbjuder ett mer lättläst gränssnitt. För att installera HTTPie, kör följande kommando:

```Fish Shell
sudo apt-get install httpie
```

Sedan kan du skicka en GET-förfrågan med hjälp av följande kommando:

```Fish Shell
http GET www.example.com
```

Detta kommer att ge samma resultat som cURL, men med en annorlunda syntax.

Du kan också skicka POST-förfrågningar med både cURL och HTTPie genom att ange en body med data som ska skickas. Detta är användbart när du vill skicka formulärdata eller JSON till en server. Detta kan göras genom att använda flaggan "-d" i cURL eller "data=" i HTTPie.

## Se även

- Fish Shell documentation: https://fishshell.com/docs/current/
- cURL manual: https://curl.haxx.se/docs/manpage.html
- HTTPie documentation: https://httpie.org/docs