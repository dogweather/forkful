---
title:                "Tolka HTML"
date:                  2024-01-20T15:31:33.218707-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka HTML betyder att läsa och förstå HTML-kod så att vi kan extrahera data. Programmerare gör det för att kunna bearbeta information från webbsidor och använda den i olika sammanhang.

## Hur gör man?
Med Fish Shell kan du använda verktyg som `pup`, ett kommandoradsverktyg för att tolka HTML.

Installera `pup`:

```Fish Shell
sudo apt install pup
```

Använd `pup` för att extrahera titlar från en HTML-fil:

```Fish Shell
curl -s http://example.com | pup 'title text{}'
```

Exempelutdata:

```
Example Domain
```

Använda en CSS-selektor för att få alla länkar:

```Fish Shell
curl -s http://example.com | pup 'a attr{href}'
```

Exempelutdata:

```
http://www.iana.org/domains/example
```

## Djupdykning
HTML-tolkning i terminalen har länge handlat om att använda verktyg som `grep`, `sed`, och `awk`. Men de här verktygen är inte optimala för komplex HTML. Därför föddes verktyg som `pup`, som är mer specialiserade på HTML.

Alternativ till `pup` inkluderar `beautifulsoup` i Python och `nokogiri` i Ruby. De är båda kraftfullare, men inte lika enkla att använda direkt i terminalen som `pup`.

När det gäller implementeringsdetaljer så använder `pup` Go-programmeringsspråket och dess paket för parsing, vilket gör det snabbt och effektivt för stora dokument.

## Se även
- [pup GitHub repository](https://github.com/ericchiang/pup)
- [beautifulsoup documentation](https://www.crummy.com/software/BeautifulSoup/)
- [nokogiri website](https://nokogiri.org/)