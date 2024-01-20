---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

"## Vad & Varför?"

Att ladda ner en webbsida innebär att du hämtar sidans data till din dator eller enhet. Programmerare gör detta för att analysera webbdata, scrappa information, testa kod och mycket mer.

"## Så här fungerar det:"

Det är enkelt att ladda ner en webbplats med Python. Vi använder `requests` biblioteket för att göra en HTTP-förfrågan. Här är hur du gör: 

``` python
import requests
svar = requests.get('http://www.google.se')
print(svar.text)
```

Kör ovanstående kod, och du lägger märke till att du har utskriften från http://www.google.se webbsidan på din terminal.

"## Fördjupning"

- **Historiska sammanhang**: Tidigare användes tekniker som telnet för att ladda ner webbsidor. Python har förenklat denna process genom tillägg av bibliotek som `requests`.
- **Alternativ**: Det finns flera andra sätt att ladda ner en webbsida. Du kan till exempel använda `urllib` i stället för `requests`. Du kan också använda verktyg som `curl` utanför Python världen.
- **Implementeringsdetaljer om att ladda ner en webbsida**: När du gör en HTTP-förfrågan med `requests.get()`, gör Python en GET-begäran till den angivna webbplatsen, laddar ner dess innehåll och returnerar det som en sträng.

"## Se även"

- [Requests: HTTP för människor](https://requests.readthedocs.io/en/master/)
- [Python Öppna en URL](https://docs.python.org/3/howto/urllib2.html)
- [Python webb skrapning tutorial](https://realpython.com/python-web-scraping-practical-introduction/)