---
date: 2024-01-26 04:30:40.022980-07:00
description: "Mit XML zu arbeiten bedeutet, Daten in einem weit verbreiteten, strukturierten\
  \ Format zu handhaben, das in Konfigurationen, Nachrichten\xFCbermittlungen und\u2026"
lastmod: '2024-03-13T22:44:54.332973-06:00'
model: gpt-4-0125-preview
summary: "Mit XML zu arbeiten bedeutet, Daten in einem weit verbreiteten, strukturierten\
  \ Format zu handhaben, das in Konfigurationen, Nachrichten\xFCbermittlungen und\
  \ mehr verwendet wird."
title: Arbeiten mit XML
weight: 40
---

## Was & Warum?
Mit XML zu arbeiten bedeutet, Daten in einem weit verbreiteten, strukturierten Format zu handhaben, das in Konfigurationen, Nachrichtenübermittlungen und mehr verwendet wird. Programmierer manipulieren XML, um Daten zu lesen, zu schreiben, zu aktualisieren und abzufragen – entscheidend für die Interoperabilität in unzähligen Apps und Diensten.

## Wie:
Fish hat keine eingebaute XML-Parserfunktion, daher müssen Sie sich auf externe Tools wie `xmllint` oder `xmlstarlet` stützen. Hier ein Schnipsel, um Werte zu lesen:

```fish
# XML mit xmlstarlet parsen
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Ausgabe:
```
Hello World
```

Um XML zu bearbeiten, verwenden Sie dies:

```fish
# XML-Element mit xmlstarlet bearbeiten
echo '<root><element>Alter Wert</element></root>' | xmlstarlet ed -u "/root/element" -v 'Neuer Wert'
```

Ausgabe:
```xml
<?xml version="1.0"?>
<root>
  <element>Neuer Wert</element>
</root>
```

## Tiefere Einblicke:
XML gibt es seit den späten 90ern, entwickelt für Lesbarkeit und Maschinenfreundlichkeit. Obwohl JSON aufgrund seiner Einfachheit etwas von der Beliebtheit von XML übernommen hat, bleibt XML dort verwurzelt, wo Dokumentenvalidierung und Namensräume Schlüsselaspekte sind.

Alternativen? Sicher – JSON, YAML oder sogar binäre Formate wie Protocol Buffers für jene leistungsintensiven Apps. Aber XMLs Schema und XSLT (für XML-Transformationen) können ausschlaggebende Faktoren in komplexen Szenarios sein, wo Robustheit zählt.

Unter der Haube nutzen Tools wie `xmlstarlet` leistungsstarke Bibliotheken wie libxml2, die Ihnen XPath und XQuery für feingranulare XML-Anpassungen an die Hand geben. Diese sind nicht nur XML-Werkzeuge, sondern Tore zur DOM-Manipulation, da Sie ähnliche Konzepte in jeder Sprache anwenden würden, die XML berührt.

## Siehe auch:
- [xmlstarlet Dokumentation](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Fish Dokumentation](https://fishshell.com/docs/current/index.html)
- [XPath and XQuery Funktionen und Operatoren](https://www.w3.org/TR/xpath-functions/)
