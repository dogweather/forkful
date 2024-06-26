---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:11.748235-07:00
description: "Wie geht das: Die Fish Shell ist vorwiegend nicht daf\xFCr ausgelegt,\
  \ HTML direkt zu parsen. Sie zeichnet sich jedoch durch das Verkn\xFCpfen von Unix-Tools\
  \ wie\u2026"
lastmod: '2024-03-13T22:44:54.307363-06:00'
model: gpt-4-0125-preview
summary: "Die Fish Shell ist vorwiegend nicht daf\xFCr ausgelegt, HTML direkt zu parsen."
title: HTML parsen
weight: 43
---

## Wie geht das:
Die Fish Shell ist vorwiegend nicht dafür ausgelegt, HTML direkt zu parsen. Sie zeichnet sich jedoch durch das Verknüpfen von Unix-Tools wie `curl`, `grep`, `sed`, `awk` aus oder durch die Verwendung von spezialisierten Tools wie `pup` oder `beautifulsoup` in einem Python-Skript. Unten sind Beispiele aufgeführt, die zeigen, wie diese Tools innerhalb der Fish Shell genutzt werden können, um HTML zu parsen.

### Verwendung von `curl` und `grep`:
HTML-Inhalte abrufen und Zeilen extrahieren, die Links enthalten:

```fish
curl -s https://example.com | grep -oP '(?<=href=")[^"]*'
```

Ausgabe:
```
/page1.html
/page2.html
...
```

### Verwendung von `pup` (ein Befehlszeilen-Tool zum Parsen von HTML):
Stellen Sie zunächst sicher, dass `pup` installiert ist. Dann können Sie es verwenden, um Elemente nach ihren Tags, IDs, Klassen usw. zu extrahieren.

```fish
curl -s https://example.com | pup 'a attr{href}'
```

Die Ausgabe ähnelt dem `grep`-Beispiel und würde href-Attribute von `<a>`-Tags auflisten.

### Mit einem Python-Skript und `beautifulsoup`:
Während die Fish-Shell an sich kein HTML nativ parsen kann, integriert sie sich nahtlos mit Python-Skripten. Unten ist ein prägnantes Beispiel, das Python mit `BeautifulSoup` verwendet, um Titel aus HTML zu parsen und zu extrahieren. Stellen Sie sicher, dass `beautifulsoup4` und `requests` in Ihrer Python-Umgebung installiert sind.

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

Verwendung:

```fish
parse_html 'https://example.com'
```

Ausgabe:
```
Beispiel Domäne
```

Jede dieser Methoden dient verschiedenen Anwendungsfällen und Komplexitätsstufen, von einfacher Befehlszeilentextmanipulation bis zur vollen Parsing-Leistung von `beautifulsoup` in Python-Skripten. Je nach Ihren Bedürfnissen und der Komplexität der HTML-Struktur können Sie eine einfache Unix-Pipeline oder einen leistungsfähigeren Skripting-Ansatz wählen.
