---
title:                "Ladda ner en webbsida"
html_title:           "Bash: Ladda ner en webbsida"
simple_title:         "Ladda ner en webbsida"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att ladda ner en webbsida innebär att du hämtar dess HTML-kod. Programmerare gör detta för att analysera innehållet, skrapa data eller testa webbintegration.

## Hur man gör:

Nedan är ett grundläggande exempel på att använda curl för att ladda ner en webbsida i Bash:

```Bash 
# Ladda ner en webbsida
curl http://www.example.com -o example.html
```

Efter detta kommando kommer `example.html` att innehålla HTML-koden för sidan `www.example.com`.

## Djupdykning

Historiskt sett har `wget` varit standardvalet för nedladdning av webbsidor i Linux-miljöer, men `curl` har blivit allt populärare på grund av dess flexibilitet och kraftfulla funktioner. Ett alternativ till dessa klassiska verktyg är att skriva skript i högre programmeringsspråk som Python eller JavaScript.

Implementeringen av nedladdning av webbsida i Bash är rakt fram. `curl` eller `wget` skickar en HTTP GET-förfrågan till den angivna URL:en och sparar svaret till den angivna filen.

Här är ett exempel med `wget`:

```Bash
# Ladda ner en webbsida med wget
wget http://www.example.com -O example.html
```

Den nedladdade webbsidan kommer att finnas i `example.html` i din nuvarande katalog.

## Se Även

För mer detaljerad information och fler exempel kan du kolla dokumentationen för `curl` (https://curl.se/docs/) och `wget` (https://www.gnu.org/software/wget/manual/wget.html). 

För ett bredare perspektiv på webbskrapning och datainsamling kan du läsa denna guiden: https://www.freecodecamp.org/news/the-ultimate-guide-to-web-scraping-with-node-js-daa2027dcd3/