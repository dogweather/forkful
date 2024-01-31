---
title:                "HTML parsen"
date:                  2024-01-20T15:30:22.729991-07:00
simple_title:         "HTML parsen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/parsing-html.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen von HTML bedeutet, den Code einer Webseite zu analysieren, um spezifische Informationen zu extrahieren. Programmierer tun dies, um Daten zu sammeln oder zu bearbeiten, oft für Aufgaben wie das Scraping von Webinhalten.

## So geht's:
Um HTML in Bash zu parsen, nutzen wir meist Tools wie `grep`, `sed`, `awk`, oder spezialisierte Parser wie `xmllint` und `pup`. Hier ein einfaches Beispiel mit `grep`:

```Bash
echo '<div>Beispiel</div>' | grep -oP '(?<=<div>).*(?=</div>)'
```
Ausgabe:
```
Beispiel
```

Für etwas mehr Struktur verwenden wir `xmllint`:

```Bash
echo '<div><p>Beispieltext</p></div>' | xmllint --html --xpath '//p/text()' -
```
Ausgabe:
```
Beispieltext
```

## Tiefergehender Einblick
Das Parsen von HTML mit Bash-Werkzeugen war nie ideal. Historisch gesehen war HTML noch nie konsequent formatiert, und simple Text-Tools hatten Schwierigkeiten mit komplexen Strukturen. Alternativen wie `python` mit `BeautifulSoup` oder `nodejs` mit `cheerio` sind robuster und bieten mehr Funktionalität. Dennoch können Tools wie `xmllint` und `pup` einfache Scraping-Aufgaben effizient erledigen und sind für kleinere Scripts oder auf Systemen ohne zusätzliche Software nützlich.

## Siehe auch:
- [https://www.gnu.org/software/grep/manual/grep.html](https://www.gnu.org/software/grep/manual/grep.html) - GNU `grep`
- [https://www.w3.org/TR/xpath/](https://www.w3.org/TR/xpath/) - `xmllint` und XPath
- [https://github.com/ericchiang/pup](https://github.com/ericchiang/pup) - `pup`, ein Befehlszeilen-Tool für HTML-Parsing
- [https://www.crummy.com/software/BeautifulSoup/](https://www.crummy.com/software/BeautifulSoup/) - `BeautifulSoup` für Python
- [https://cheerio.js.org/](https://cheerio.js.org/) - `cheerio` für Node.js
