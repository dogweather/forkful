---
title:                "Skicka en http-begäran"
html_title:           "Python: Skicka en http-begäran"
simple_title:         "Skicka en http-begäran"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-förfrågan är en viktig del av programmering eftersom det låter dig kommunicera med en server och hämta data eller utföra en åtgärd. Det är den grundläggande metoden för att ansluta till och använda webbplatser och webbapplikationer.

## Så här gör du:

```python
import requests

# Skicka en förfrågan till Google och skriv ut statuskoden
response = requests.get('https://www.google.com')
print(response.status_code)
```

```python
import requests

# Skicka en POST-förfrågan med en JSON-payload
data = {'username': 'John', 'password': 'secret'}
response = requests.post('https://example.com/login', json=data)
print(response.content)
```

## Djupdykning:

Att skicka HTTP-förfrågningar har funnits sedan början av World Wide Web. Det finns också andra protokoll som kan användas för att skicka förfrågningar, som till exempel FTP och SMTP. Det finns också olika bibliotek som kan användas för att skicka HTTP-förfrågningar i Python, såsom urllib och httplib. Requests-biblioteket är dock ett populärt alternativ på grund av dess enkla syntax och användarvänlighet.

## Se även:

- [Requests documentation](https://requests.readthedocs.io/en/master/)
- [Python standard library: urllib](https://docs.python.org/3/library/urllib.html)
- [Python standard library: httplib](https://docs.python.org/3/library/http.client.html)