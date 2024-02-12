---
title:                "Tolka HTML"
aliases: - /sv/fish-shell/parsing-html.md
date:                  2024-02-03T19:12:19.925987-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tolka HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka (parse:a) HTML handlar om att extrahera data eller information från HTML-innehåll, en vanlig uppgift när man arbetar med webbdata. Programmerare gör detta för att automatisera extraktionen av information från webbplatser, för uppgifter som webbskrapning, datautvinning eller automatiserade tester.

## Hur:

Fish shell är huvudsakligen inte designat för att direkt parse:a HTML. Däremot är det utmärkt på att sammanfoga Unix-verktyg som `curl`, `grep`, `sed`, `awk`, eller att använda specialiserade verktyg som `pup` eller `beautifulsoup` i ett Python-skript. Nedan följer exempel som visar hur du kan utnyttja dessa verktyg inom Fish shell för att parse:a HTML.

### Använda `curl` och `grep`:
Hämta HTML-innehåll och extrahera rader som innehåller länkar:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

Utdata:
```
/page1.html
/page2.html
...
```

### Använda `pup` (ett kommandoradsverktyg för att tolka HTML):

Först, se till att `pup` är installerat. Sedan kan du använda det för att extrahera element med deras taggar, id:n, klasser, osv.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

Utdata, liknande `grep`-exemplet, skulle lista href-attributen för `<a>`-taggar.

### Med ett Python-skript och `beautifulsoup`:

Medan Fish i sig inte kan parse:a HTML på ett infött sätt, integreras det sömlöst med Python-skript. Nedan är ett koncist exempel som använder Python med `BeautifulSoup` för att parse:a och extrahera titlar från HTML. Se till att du har `beautifulsoup4` och `requests` installerat i din Python-miljö.

**parse_html.fish**

```fish
function parse_html -a url
    python -c "
import sys
import requests
from bs4 import BeautifulSoup

response = requests.get(sys.argv[1])
soup = BeautifulSoup(response.text, 'html.parser')

titles = soup.find_all('title')

for title in titles:
    print(title.get_text())
" $url
end
```

Användning:

```fish
parse_html 'https://example.com'
```

Utdata:
```
Exempeldomän
```

Var och en av dessa metoder tjänar olika användningsområden och komplexitetsnivåer, från enkel kommandorads textmanipulering till full parsing-kraft med `beautifulsoup` i Python-skript. Beroende på dina behov och HTML-strukturens komplexitet, kan du välja en enkel Unix-pipelining eller ett kraftfullare skriptningsansats.
