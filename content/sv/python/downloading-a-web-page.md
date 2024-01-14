---
title:                "Python: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Varför

Att ladda ner en webbsida är en användbar färdighet inom Python-programmering, särskilt om man vill automatisera uppgifter eller extrahera data från internet.

# Hur man gör det

För att ladda ner en webbsida i Python, kan man använda biblioteket "requests". Först måste man installera det genom att importera: 

```Python 
import requests 
```

Sedan kan man använda funktionen "get()" för att hämta in webbsidan genom att ange dess URL som ett argument. Till exempel: 

```Python 
r = requests.get("https://www.example.com")
```

För att se det innehåll som har hämtats kan man använda funktionen "text". Till exempel: 

```Python 
print(r.text)
```

Detta kommer att skriva ut alla HTML-koden för den hämtade webbsidan. Man kan också välja att spara detta innehåll i en fil genom att använda funktionen "write()". Till exempel: 

```Python 
with open("example.html", "w") as file: 
    file.write(r.text) 
```

Detta är en enkel metod för att ladda ner en webbsida, men det finns många andra sätt att göra det på och biblioteket "requests" har många fler funktioner för att anpassa hämtningar.

# Gå i djupet

För att förstå ytterligare om hur man laddar ner en webbsida i Python, är det bra att ha en grundläggande förståelse för HTTP-protokollet. Detta är det protokoll som används för att skicka och ta emot data över internet. När man använder funktionen "get()" i "requests"-biblioteket, så skickas faktiskt en GET-begäran till webbsidan och dess svar returneras som ett objekt. Detta objekt, som vi i exemplet kallade för "r", innehåller en mängd olika attribut och funktioner för att hantera den hämtade webbplatsen. Det är värt att undersöka och förstå dessa för att använda "requests"-biblioteket mer effektivt.

# Se även

- [Dokumentation för requests-biblioteket](https://requests.readthedocs.io/en/master/)
- [Mer information om HTTP-protokollet](https://www.w3.org/Protocols/HTTP/)
- [Tutorial om webb-hämtningar med Python](https://realpython.com/python-web-scraping-practical-introduction/)