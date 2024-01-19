---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta en webbsida innebär att ladda ner webbsidans HTML till din enhet. Programmerare gör detta för att analysera, manipulera eller visa webbsidans data.

## Så här gör du:
Med hjälp av curl i Fish Shell kan vi enkelt hämta en webbsida. Ditt skript kan se ut så här:

```Fish Shell
function hämta-webbsida
  set url $argv[1]
  curl $url -o "webbsida.html"
end
```

Kör kommandot så här:

```Fish Shell
hämta-webbsida https://google.com
```

Output resultatet kommer att bli en fil kallad "webbsida.html" i din aktuella katalog.

## Djupdykning
Historiskt sett har HTML-läsning använts för webbskrapning och analys av webbdata. Alternativa metoder för att hämta en webbsida inkluderar bibliotek som Scrapy i Python, eller användning av HTTP/S-anrop i JavaScript.

Implementationen i Fish är ganska enkel, curl-kommando är ett kraftfullt verktyg som hanterar ett komplett HTTP GET-request och sparar output till "webbsida.html". "argv[1]" i koden står för den första argumentet (i detta fall URL), vilket gör funktionen flexibel för olika URL:er.

## Se också
- Fish Shell Dokumentation: https://fishshell.com/docs/current/index.html
- Curl Manual: https://curl.se/docs/manpage.html
- Scrapy Dokumentation: https://docs.scrapy.org/en/latest/
- Fetch API i JavaScript: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API