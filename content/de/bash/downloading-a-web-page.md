---
title:                "Webseite herunterladen"
date:                  2024-01-20T17:43:15.922411-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite bedeutet, ihren Inhalt auf deinen Rechner zu ziehen. Programmierer machen das, um Daten zu analysieren, Backups zu erstellen oder um Inhalte offline verfügbar zu machen.

## How to:
```Bash
# Mit curl eine Webseite herunterladen
curl http://example.com -o meine_webseite.html

# Ausgabe kurzer Check
cat meine_webseite.html
```

```Bash
# Mit wget eine ganze Seite inklusive Ressourcen herunterladen
wget --mirror --convert-links --adjust-extension --page-requisites --no-parent http://example.com

# Liste der heruntergeladenen Dateien
ls example.com
```

## Deep Dive
Früher, als die Internetverbindung langsamer und weniger stabil war, haben Entwickler Webseiten oft heruntergeladen, um in Ruhe daran zu arbeiten. `curl` und `wget` sind die zwei Schwergewichte für diese Aufgabe. `curl` ist praktisch, um einzelne Dateien schnell zu schnappen. `wget` hingegen ist ein Power-Tool, das ganze Webseiten rekursiv herunterladen kann. Beide Tools können auch mit Skripten verwendet werden, um automatisierte Downloads zu erstellen.

Alternative Methoden beinhalten Web Scraping mit spezialisierten Sprachen und Tools wie Python mit Bibliotheken wie `requests` und `BeautifulSoup` für komplexere Aufgaben. Im Kern geht es darum, eine Kopie der Webseite in ihrer aktuellen Form zu erhalten – ob nur der HTML-Text oder die gesamte Struktur, mit Bildern und Stylesheets, hängt vom Bedarf ab.

## See Also
- curl Dokumentation: https://curl.se/docs/
- wget Manual: https://www.gnu.org/software/wget/manual/wget.html
- Einführung in Web Scraping mit Python: https://realpython.com/python-web-scraping-practical-introduction/
