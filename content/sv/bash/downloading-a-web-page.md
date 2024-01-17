---
title:                "Nerladdning av en webbsida"
html_title:           "Bash: Nerladdning av en webbsida"
simple_title:         "Nerladdning av en webbsida"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att ladda ner en webbsida är helt enkelt att hämta innehållet från en viss webbadress och spara det på din dator. Det är en vanlig uppgift för programmerare, eftersom det kan vara användbart för att hämta information från en webbplats eller automatisera vissa uppgifter.

## Hur man:

```Bash
# Hämta innehållet från en webbsida och spara det i en fil
curl https://www.example.com > fil.html

# Hämta bara innehållet från en webbsida och skriv ut det på skärmen
curl https://www.example.com

# Hämta filen till en specifik sökväg på din dator
curl -o /path/to/file.html https://www.example.com

# Hämta innehållet från flera webbplatser samtidigt
curl -o fil1.html https://www.example1.com -o fil2.html https://www.example2.com
```

För att kontrollera om din kod fungerar kan du använda `cat`kommandot för att visa innehållet i den hämtade filen. Till exempel `cat fil.html` för att visa innehållet i filen du hämtade ovan.

## Djupdykning:

Att ladda ner en webbsida är en viktig del av webbutveckling och automatisering. Tidigare var det vanligt att använda programmet `wget` för att ladda ner webbsidor, men idag är `curl`mer vanligt förekommande.

Det finns också alternativ till `curl`, till exempel programspråk som Python eller JavaScript har inbyggda bibliotek för att hämta webbsidor. Men `curl`är fortfarande vanligtvis det mest använda verktyget för denna uppgift, eftersom det är enkelt att använda och fungerar på många olika operativsystem.

## Se även:

- [Tutorial: Downloading Files with Curl](https://linuxize.com/post/curl-download-file/)
- [Curl's Official Website](https://curl.haxx.se/)
- [Curl's Manual Page](https://curl.haxx.se/docs/manpage.html)