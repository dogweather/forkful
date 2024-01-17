---
title:                "HTML parsen"
html_title:           "Bash: HTML parsen"
simple_title:         "HTML parsen"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum? 
Parsing von HTML ist der Prozess des Extrahierens und Verarbeitens von Daten aus einer HTML-Datei. Programmierer verwenden das Parsing von HTML, um strukturierte Daten aus Webseiten zu gewinnen, die dann in ihren Codes verwendet oder analysiert werden können.

## Wie gehts?
Um HTML mit Bash zu parsen, kann das 'lynx' Tool verwendet werden. Um eine HTML-Datei namens 'index.html' zu parsen und die extrahierten Daten in einer Datei namens 'output.txt' zu speichern, können Sie den folgenden Befehl verwenden:
```Bash
lynx -dump -listonly index.html > output.txt
``` 
Dieser Befehl erzeugt eine Textdatei mit allen Links und dem entsprechenden Text aus der HTML-Datei.

## Tiefere Einblicke
Parsing von HTML hat eine lange Geschichte und ist ein wichtiger Bestandteil der Webentwicklung. Es gibt auch alternative Tools wie 'grep', 'awk' oder 'sed', die für das Parsen von HTML-Dateien verwendet werden können. Bei der Implementierung von HTML-Parsing müssen jedoch einige Dinge beachtet werden, wie z.B. die Handhabung von Sonderzeichen und die Behandlung von Fehlerfällen.

## Siehe auch
- [Bash Dokumentation] (https://www.gnu.org/software/bash/manual/bash.html)
- [Lynx Website](http://lynx.browser.org/)
- [Informationen über HTML Parsing] (https://www.w3schools.com/html/html_parse.asp)