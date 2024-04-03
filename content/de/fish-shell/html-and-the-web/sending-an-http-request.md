---
date: 2024-01-20 17:59:30.313371-07:00
description: "HTTP-Anfragen sind das Herzst\xFCck des Webs \u2013 sie erm\xF6glichen\
  \ die Kommunikation zwischen dem Client (dein Rechner) und dem Server. Programmierer\
  \ nutzen sie,\u2026"
lastmod: '2024-03-13T22:44:54.306424-06:00'
model: gpt-4-1106-preview
summary: "HTTP-Anfragen sind das Herzst\xFCck des Webs \u2013 sie erm\xF6glichen die\
  \ Kommunikation zwischen dem Client (dein Rechner) und dem Server."
title: Einen HTTP-Request senden
weight: 44
---

## Was & Warum?
HTTP-Anfragen sind das Herzstück des Webs – sie ermöglichen die Kommunikation zwischen dem Client (dein Rechner) und dem Server. Programmierer nutzen sie, um Daten zu holen, zu senden, und Webdienste zu nutzen.

## Anleitung:
Mit `curl`, einem mächtigen Werkzeug am Terminal, sendest du HTTP-Anfragen schnell und unkompliziert.

```
Fish Shell
# Einfacher GET-Request
curl http://example.com

# POST-Request mit Daten
curl -d "param1=value1&param2=value2" -X POST http://example.com

# Anzeigen der Response-Header
curl -I http://example.com
```

Sample Output:

```
HTML-Code der Webseite example.com...
```

## Tiefer Eintauchen:
Früher taten sich Entwickler schwer mit komplexen, plattformabhängigen Möglichkeiten, um HTTP-Anfragen zu senden – dann kam `curl` in den späten 90ern. Es ist portabel, mächtig und in nahezu jeder Unix-basierten Umgebung vorhanden, inklusive Fish Shell. Alternativen wie `wget` oder Programmiersprachen-spezifische Libraries (wie Python `requests`) sind auch verbreitet, doch `curl` ist oft die erste Wahl für einfache Aufgaben. Bei der Implementierung liegt der Teufel im Detail, wie Header-Manipulationen oder Verarbeitung von Cookies, die `curl` aber leicht macht.

## Siehe Auch:
- Die offizielle `curl`-Dokumentation: [https://curl.se/docs/manpage.html](https://curl.se/docs/manpage.html)
- Mehr zu HTTP-Anfragen und Methoden: [https://developer.mozilla.org/en-US/docs/Web/HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
- Fish Shell Documentation: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
