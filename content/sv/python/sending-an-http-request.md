---
title:                "Python: Sända en http-förfrågan"
simple_title:         "Sända en http-förfrågan"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Varför

Att kunna skicka HTTP-förfrågningar i din Python-kod är en viktig färdighet för alla programmerare. Det tillåter dig att kommunicera med externa API:er och webbservrar, vilket möjliggör interaktion mellan din kod och andra webbapplikationer. 

## Hur man gör

För att skicka en HTTP-förfrågan i Python behöver du importera "requests" biblioteket. Det är en populär och användarvänlig bibliotek som gör det enkelt att göra förfrågningar. 

```Python
import requests

response = requests.get("https://api.github.com")
print(response.status_code)
```

Den enklaste formen av en HTTP-förfrågan är en GET-förfrågan som vi använder i exemplet ovan. Vi lagrar svaret från förfrågan i en variabel "response" och skriver ut statuskoden med hjälp av "status_code" metoden.

## Djupdykning

HTTP-förfrågningar fungerar genom ett klient-servrar-system, där Python-koden fungerar som klienten som skickar en förfrågan till en webbserver. För att skicka en förfrågan behöver vi en URL som motsvarar den webbadress vi vill ansluta till. Det finns olika typer av HTTP-förfrågningar som tillåter oss att göra mer än bara att hämta data från en annan webbplats. Till exempel kan vi använda en POST-förfrågan för att skicka data till en server, eller en PUT-förfrågan för att uppdatera befintlig data. 

En annan viktig del av en HTTP-förfrågan är dess statuskod. Detta är ett numeriskt svar från servern som anger om förfrågan var framgångsrik eller inte. I vårt exempel ovan ser vi att statuskoden är 200, vilket betyder att vår förfrågan lyckades. Om statuskoden är något annat än 200 kan det indikera att det uppstod ett problem med vår förfrågan.

## Se även

- [Dokumentation för requests biblioteket](https://requests.readthedocs.io/en/master/)
- [En guide till HTTP-förfrågningar i Python](https://realpython.com/python-requests/) 
- [En introduktion till HTTP-protokollet](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)