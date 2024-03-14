---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:24.574945-07:00
description: "HTML zu parsen bedeutet, die Struktur und den Inhalt einer HTML-Datei\
  \ zu durchforsten, um Informationen zu extrahieren. Programmierer tun dies, um auf\u2026"
lastmod: '2024-03-13T22:44:54.057381-06:00'
model: gpt-4-0125-preview
summary: "HTML zu parsen bedeutet, die Struktur und den Inhalt einer HTML-Datei zu\
  \ durchforsten, um Informationen zu extrahieren. Programmierer tun dies, um auf\u2026"
title: HTML parsen
---

{{< edit_this_page >}}

## Was & Warum?

HTML zu parsen bedeutet, die Struktur und den Inhalt einer HTML-Datei zu durchforsten, um Informationen zu extrahieren. Programmierer tun dies, um auf Daten zuzugreifen, Inhalte zu manipulieren oder Websites zu scrapen.

## Wie:

Bash ist nicht die erste Wahl zum Parsen von HTML, aber es kann mit Werkzeugen wie `grep`, `awk`, `sed` oder externen Dienstprogrammen wie `lynx` durchgeführt werden. Für Robustheit verwenden wir `xmllint` aus dem `libxml2` Paket.

```bash
# Installiere xmllint, falls nötig
sudo apt-get install libxml2-utils

# Beispiel HTML
cat > sample.html <<EOF
<html>
<head>
  <title>Beispielseite</title>
</head>
<body>
  <h1>Hallo, Bash!</h1>
  <p id="myPara">Bash kann mich lesen.</p>
</body>
</html>
EOF

# Den Titel parsen
title=$(xmllint --html --xpath '//title/text()' sample.html 2>/dev/null)
echo "Der Titel ist: $title"

# Absatz nach ID extrahieren
para=$(xmllint --html --xpath '//*[@id="myPara"]/text()' sample.html 2>/dev/null)
echo "Der Absatzinhalt ist: $para"
```

Ausgabe:
```
Der Titel ist: Beispielseite
Der Absatzinhalt ist: Bash kann mich lesen.
```

## Vertiefung

Früher verwendeten Programmierer regex-basierte Tools wie `grep` zum Scannen von HTML, aber das war umständlich. HTML ist nicht regulär – es ist kontextuell. Traditionelle Werkzeuge verpassen dies und können fehleranfällig sein.

Alternativen? Genug. Python mit Beautiful Soup, PHP mit DOMDocument, JavaScript mit DOM-Parsern – Sprachen mit Bibliotheken, die darauf ausgelegt sind, die Struktur von HTML zu verstehen.

Die Verwendung von `xmllint` in Bash-Skripten ist fest für einfache Aufgaben. Es versteht XML und somit auch XHTML. Reguläres HTML kann jedoch unvorhersehbar sein. Es folgt nicht immer den strengen Regeln von XML. `xmllint` zwingt HTML in ein XML-Modell, was gut für wohlgeformtes HTML funktioniert, aber bei unordentlichem Material ins Straucheln geraten kann.

## Siehe auch

- [W3Schools - HTML DOM Parser](https://www.w3schools.com/xml/dom_intro.asp): Demystifiziert HTML DOM.
- [MDN Web Docs - Parsen und Serialisieren von XML](https://developer.mozilla.org/en-US/docs/Web/Guide/Parsing_and_serializing_XML): Für XML-Parsing-Prinzipien, die für XHTML gelten.
- [Beautiful Soup Dokumentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/): Eine Python-Bibliothek für das Parsen von HTML.
- [libxml2 Dokumentation](http://xmlsoft.org/): Details zu `xmllint` und verwandten XML-Werkzeugen.
