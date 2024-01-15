---
title:                "Sänder en http-begäran"
html_title:           "Python: Sänder en http-begäran"
simple_title:         "Sänder en http-begäran"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att skicka HTTP-förfrågningar är en grundläggande färdighet för programmerare som vill skapa webbapplikationer eller arbeta med API:er. Genom att förstå hur HTTP-protokollet fungerar kan du kommunicera med servrar och hämta eller skicka data på webben.

## Hur man gör

För att skicka en HTTP-förfrågan i Python, behöver du importera inbyggda urllib-biblioteket:

```Python
import urllib.request
```

För att skicka en GET-förfrågan, använd `urlopen()`-funktionen:

```Python
url = "http://exempelwebbplats.com"
response = urllib.request.urlopen(url)
```

Du kan sedan läsa svaret som en sträng med `read()`-metoden:

```Python
data = response.read()
```
För att skicka en POST-förfrågan, behöver du först koda dina data med `urllib.parse`-modulen:

```Python
import urllib.parse
data = urllib.parse.urlencode({'key': 'value'})
data = data.encode('utf-8') # om du behöver konvertera till byte-format
```

Sedan kan du skicka förfrågan med `urlopen()`-funktionen och ange dina data som argument:

```Python
url = "http://exempelwebbplats.com"
response = urllib.request.urlopen(url, data)
```

## Djupdykning

HTTP står för "Hypertext Transfer Protocol" och är ett protokoll som möjliggör kommunikation mellan klienter och servrar på webben. Det använder en "request-response"-modell där en klient skickar en förfrågan till en server, som sedan svarar med en respons. En HTTP-förfrågan består av en metod (t.ex. GET eller POST), en URL och en valfri "body" som kan innehålla data.

Det finns också olika typer av statuskoder som en server kan skicka tillbaka i en HTTP-respons. Exempelvis, om allt gick bra kommer du få ett 200-kod, men om det finns ett fel kommer du få en annan kod för att ange vad som gick fel.

Att lära sig mer om HTTP och dess specifikationer kan hjälpa dig att förstå hur du skickar och tar emot förfrågningar på ett effektivt sätt.

## Se även

- [Python's urllib-dokumentation](https://docs.python.org/3/library/urllib.html)
- [Hypertext Transfer Protocol - Wikipedia](https://sv.wikipedia.org/wiki/Hypertext_Transfer_Protocol)
- [HTTP statuskoder - MDN webbdokumentation](https://developer.mozilla.org/sv-SE/docs/Web/HTTP/Status)