---
title:                "Arbeiten mit XML"
date:                  2024-01-26T04:30:49.043836-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit XML umfasst das Parsen, Manipulieren und Generieren von XML-Dokumenten, die aufgrund ihres strukturierten und weit verbreiteten Formats für den Datenaustausch verwendet werden. Programmierer handhaben XML, um mit unzähligen Systemen zu interagieren, bei denen XML die Lingua Franca der Daten ist.

## Wie zu:
Gleam unterstützt XML nicht nativ, daher verwenden wir eine externe Bibliothek wie `gleam_xml`. Fügen Sie diese zunächst Ihrer `gleam.toml` hinzu:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Nun, parsen und erstellen Sie XML:

```rust
import gleam/xml

// Parse XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Erstelle XML
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")]),
  ]
)
let xml_string = xml.render(node)
```

Beispielausgabe für `xml.render(node)` ist:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Tiefergehend
XML steht für eXtensible Markup Language, eine Spezifikation des W3C als Schwester von HTML. Es existiert seit den späten 90er Jahren. Für Gleam fühlt sich die Handhabung von XML ein bisschen wie ein Schritt zurück in der Zeit an. JSON und Protocol Buffers sind trendiger, aber die umfangreiche Verwendung von XML in Altsystemen und bestimmten Branchen bedeutet, dass es immer noch relevant ist.

Alternativen wie `xmerl` existieren im Erlang-Ökosystem; jedoch bietet die Bibliothek `gleam_xml` einen für Gleam-Benutzer idiomatischeren Ansatz. Sie basiert auf vorhandenen Erlang-Bibliotheken, stellt jedoch eine Gleam-freundliche API zur Verfügung. Der Gleam-Ansatz für XML zielt auf Einfachheit und Sicherheit ab, reduziert den Boilerplate-Code und legt Wert auf Typsicherheit.

Implementierungstechnisch bieten XML-Bibliotheken wie `gleam_xml` typischerweise DOM-ähnliche Strukturen. Dies beinhaltet Knoten, Attribute und verschachtelte Elemente und nutzt Erlangs Musterabgleich und Konkurrenzmodelle, um potenziell große und komplexe Dokumente zu handhaben.

## Siehe Auch
- Die `gleam_xml` Bibliothek auf Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Offizieller XML-Standard von W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Umfassendes XML-Tutorial: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Erlangs `xmerl` Dokumentation zur XML-Verarbeitung: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)
