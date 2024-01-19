---
title:                "Att skicka en http-begäran"
html_title:           "Go: Att skicka en http-begäran"
simple_title:         "Att skicka en http-begäran"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

HTTP-anrop är uppmaningar skickade från en klient till en server för att hämta data. Programmers behöver detta för att interagera med webbtjänster, ladda ner filer och allt däremellan.

## Hur man:

Python har ett inbyggt bibliotek som heter `requests` för att skicka HTTP-anrop. Första gången, kommer du behöva installera det med kommandot `pip install requests`.

Här är ett exempel på hur du gjör detta:

```python
import requests

response = requests.get('http://example.com')

print(response.status_code)
print(response.content)
```

När du kör detta kommer det att visa:

```
200
b'<!doctype html>...'
```

Det betyder att du har fått en HTTP 200 status kod, vilket betyder "OK", och innehållet på sidan som en `bytes`-typ.

## Djupdykning

Historiskt sett var det betydligt mer komplicerat att skicka HTTP-anrop innan `requests`-modulen skapades. Python har ett äldre modul som heter `urllib`, men det är betydligt mer komplicerat att använda.

Vad gäller alternativ till `requests`, så finns det bibliotek som `http.client` (inbyggt), `httplib2`, `treq` och många fler. De är dock generellt sett mer komplexa att använda än `requests`.

När det kommer till detaljer av implementering, skickar `requests.get()`-funktionen en `GET`-begäran till den angivna URL:en. Den returnerar ett `Response`-objekt som innehåller serverns svar på vår begäran.

## Se Även

För ytterligare undersökning, se:

- [Officiella dokument för requests](https://requests.readthedocs.io/)
- [HTTP: The Protocol Every Web Developer Must Know](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- [Python’s Requests Library](https://realpython.com/python-requests/) (tutorial)