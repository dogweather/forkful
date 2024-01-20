---
title:                "Tolka HTML"
date:                  2024-01-20T15:30:11.843567-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Vad och varför?
Parsing av HTML handlar om att extrahera specifik data från HTML-kod. Programmerare gör detta för att automatisera samlandet av information från webbsidor eller för att bearbeta innehållet.

## Hur man gör:
Att parse:a HTML kan vara klurigt med Bash eftersom det inte är designat för riktigt så komplexa strukturer. Men med verktyg som `grep`, `sed`, och `awk`, kan man göra enkla extraktioner. För bättre resultat är specialiserade verktyg som `xmllint` eller `pup` att föredra.

### Enkel extraktion med grep:
```Bash
echo '<p>Hej världen!</p>' | grep -oP '(?<=<p>).*(?=</p>)'
```
Output:
```
Hej världen!
```

### Extrahera titel med sed:
```Bash
echo '<title>Min Sida</title>' | sed -n 's/.*<title>\(.*\)<\/title>.*/\1/p'
```
Output:
```
Min Sida
```

## Fördjupning:
Bash-scriptning är inte idealisk för att parse:a HTML eftersom HTML inte är en regelbunden syntax och kan vara svårt att förutsäga. Historiskt sett har folk använt regex-verktyg som `grep`, `sed`, och `awk`, men dessa kan lätt krångla till det och är inte robusta lösningar.

Alternativ som `xmllint`, en del av `libxml2` paketet, ger en bättre konsekvens och säkerhet. Ett annat alternativ är `pup`, ett kommandoradsverktyg för HTML parsing inspirerad av `jq`.

I Bash är det viktigt att hålla sig till enkla parseringsscenarion, eller att överväga ett mer lämpligt programmeringsspråk som Python med bibliotek som Beautiful Soup när det krävs mer avancerad och detaljerad parsing.

## Se även:
- xmllint: http://xmlsoft.org/xmllint.html
- pup: https://github.com/ericchiang/pup
- Beautiful Soup dokumentation (för Python): https://www.crummy.com/software/BeautifulSoup/bs4/doc/