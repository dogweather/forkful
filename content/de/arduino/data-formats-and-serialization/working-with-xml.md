---
date: 2024-01-26 04:27:23.182934-07:00
description: "Die Arbeit mit XML auf Arduino umfasst das Parsen und Manipulieren von\
  \ XML-Daten, die \xFCblicherweise von Web-APIs oder Konfigurationsdateien kommen.\u2026"
lastmod: '2024-03-13T22:44:54.166908-06:00'
model: gpt-4-0125-preview
summary: "Die Arbeit mit XML auf Arduino umfasst das Parsen und Manipulieren von XML-Daten,\
  \ die \xFCblicherweise von Web-APIs oder Konfigurationsdateien kommen.\u2026"
title: Arbeiten mit XML
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit XML auf Arduino umfasst das Parsen und Manipulieren von XML-Daten, die üblicherweise von Web-APIs oder Konfigurationsdateien kommen. Programmierer tun dies, um sich in Dienste zu integrieren, die XML für den Datenaustausch verwenden, oder um Daten in einem strukturierten, menschenlesbaren Format zu speichern.

## Wie geht das:
Wir werden die `XMLWriter` Bibliothek verwenden, um XML zu erstellen, und die `tinyxml2` Bibliothek, um es zu parsen. Installieren Sie zuerst die Bibliotheken über den Bibliotheks-Manager in Ihrer Arduino IDE.

Ein XML-Dokument erstellen:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Verwendet Serial zur Ausgabe
  
  xml.header();
  xml.tag("greeting").tag("text").text("Hallo, Welt!").close().close();
  xml.flush();
}

void loop() {
}
```

Einen XML-String dekodieren:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>Hallo, Welt!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

Beispielausgabe:

```
<greeting>
  <text>Hallo, Welt!</text>
</greeting>
```

## Tiefergehend
XML oder Extensible Markup Language ist eine Auszeichnungssprache, die einen Satz von Regeln für die Kodierung von Dokumenten in einem Format festlegt, das sowohl menschen- als auch maschinenlesbar ist. Sie ist seit den späten 90er Jahren vorhanden und wird in verschiedenen Bereichen, insbesondere dort, wo plattformunabhängiger Datenaustausch benötigt wird, umfassend verwendet. Die begrenzten Speicherressourcen von Arduino machen die Arbeit mit XML anspruchsvoller als auf einem PC. Daher sind leichtgewichtige Bibliotheken entscheidend. Obwohl JSON aufgrund seiner einfacheren Syntax und geringeren Größe an Popularität für den Datenaustausch gewonnen hat, wird XML, besonders beim Umgang mit Altsystemen oder Anwendungen, die Dokumentenvalidierung über Schemata erfordern, immer noch weitgehend verwendet. Der Schlüssel zur Arduino XML-Implementierung ist das Stream-Parsing, das das Dokument in Segmenten liest, um den Speicherverbrauch niedrig zu halten.

## Siehe auch
- [TinyXML-2 Bibliotheksdokumentation](https://leethomason.github.io/tinyxml2/)
- [Arduino JSON-Bibliothek](https://arduinojson.org/) als Alternative bei der Arbeit mit JSON-Daten.
- [W3Schools XML-Tutorial](https://www.w3schools.com/xml/) für allgemeines XML-Lernen.
- [W3C XML-Spezifikation](https://www.w3.org/XML/) für die offiziellen XML-Standards und Empfehlungen.
