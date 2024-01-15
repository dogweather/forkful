---
title:                "Nedladdning av en webbsida"
html_title:           "Python: Nedladdning av en webbsida"
simple_title:         "Nedladdning av en webbsida"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Varför

Att ladda ner en webbsida är en vanlig uppgift inom webbutveckling och datascience. Det kan användas för att hämta information från en specifik webbsida, analysera webbsidans innehåll eller för att bygga en lokal backup av en hemsida.

## Hur man gör det

För att ladda ner en webbsida i Python behöver man använda sig av ett tredjepartsbibliotek som heter "Requests". Detta bibliotek gör det möjligt för oss att skicka HTTP-förfrågningar till en webbsida och hämta dess innehåll.

För att installera "Requests" biblioteket, kan du använda dig av pip-kommandot i terminalen:

```python
pip install requests
```

När biblioteket är installerat, kan du importera det i ditt Python-skript genom att använda följande kod:

```python
import requests
```

För att hämta en webbsida använder vi funktionen "get" från "Requests" biblioteket och anger webbadressen som en parameter. Detta skapar sedan ett responsobjekt som innehåller webbsidans innehåll.

```python
# Hämtar en webbsida
response = requests.get("https://www.example.com")

# Skriver ut webbsidans innehåll
print(response.content)
```

En vanlig uppgift är också att spara webbsidans innehåll till en lokal fil. Detta kan göras genom att använda funktionen "open" och skicka in ett filnamn tillsammans med "wb"-läget (vilket står för "write binary").

```python
# Skapar en lokal fil och sparar webbsidans innehåll
with open("exempel.html", "wb") as file:
    file.write(response.content)
```

## Djupdykning

Utöver att hämta en hel webbsida, kan man också använda "Requests" biblioteket för att hämta specifika delar av en webbsida. Detta kan göras genom att använda "BeautifulSoup" biblioteket för webbskrapning.

Först måste man installera "BeautifulSoup" genom att köra följande kommando:

```python
pip install beautifulsoup4
```

Sedan kan man importera biblioteket i sitt Python-skript:

```python
from bs4 import BeautifulSoup
```

Därefter kan man använda "BeautifulSoup" tillsammans med "Requests" för att hitta specifika HTML-element på en webbsida.

```python
# Hämtar en webbsida
response = requests.get("https://www.example.com")

# Skapar ett BeautifulSoup objekt av webbsidans innehåll
soup = BeautifulSoup(response.content, 'html.parser')

# Hittar alla "a"-taggar på webbsidan och skriver ut deras länkar
for link in soup.find_all('a'):
    print(link.get('href'))
```

Detta är bara en grundläggande introduktion till att ladda ner webbsidor i Python. Det finns många fler aspekter som kan utforskas, som att hantera HTTP-headers, skicka POST-förfrågningar och autentisering.

## Se även

- [Dokumentation för Requests biblioteket](https://requests.readthedocs.io/en/master/)
- [Dokumentation för BeautifulSoup biblioteket](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) 
- [En guide till webbskrapning med Python](https://realpython.com/beautiful-soup-web-scraper-python/)