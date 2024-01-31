---
title:                "Arbeiten mit XML"
date:                  2024-01-26T04:30:40.022980-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/working-with-xml.md"
---

{{< edit_this_page >}}

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
