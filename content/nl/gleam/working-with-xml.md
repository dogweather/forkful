---
title:                "Werken met XML"
date:                  2024-01-28T22:11:21.126186-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/gleam/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Werken met XML omvat het parsen, manipuleren en genereren van XML-documenten, die worden gebruikt voor gegevensuitwisseling vanwege hun gestructureerde en wijdverspreide formaat. Programmeurs hanteren XML om te interfacing met talloze systemen waar XML de lingua franca van gegevens is.

## Hoe:
Gleam ondersteunt van nature geen XML, dus we zullen een externe bibliotheek zoals `gleam_xml` gebruiken. Voeg het eerst toe aan je `gleam.toml`:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Nu, parseer en creëer XML:

```rust
import gleam/xml

// Parseer XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Creëer XML
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")]),
  ]
)
let xml_string = xml.weergeven(node)
```

Voorbeelduitvoer voor `xml.weergeven(node)` is:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Diepgaande Duik
XML staat voor eXtensible Markup Language, een specificatie van het W3C als een zuster van HTML. Het is al rond sinds de late jaren '90. Voor Gleam voelt het omgaan met XML een beetje als een stap terug in de tijd. JSON en Protocol Buffers zijn trendier, maar het uitgebreide gebruik van XML in legacy-systemen en bepaalde industrieën betekent dat het nog steeds relevant is.

Alternatieven zoals `xmerl` bestaan in het Erlang-ecosysteem; echter, de `gleam_xml` bibliotheek biedt een meer idiomatische aanpak voor Gleam-gebruikers. Het is gebouwd op bestaande Erlang-bibliotheken maar biedt een Gleam-vriendelijke API. De Gleam-aanpak voor XML streeft naar eenvoud en veiligheid, waardoor de boilerplate wordt verminderd en de nadruk op typeveiligheid ligt.

Wat de implementatie betreft, bieden XML-bibliotheken inclusief `gleam_xml` doorgaans DOM-achtige structuren. Dit omvat nodes, attributen en geneste elementen, waarbij gebruik wordt gemaakt van Erlang's patroonmatching en concurrency modellen om potentieel grote en complexe documenten te verwerken.

## Zie Ook
- De `gleam_xml` bibliotheek op Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Officiële XML-standaard door W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Uitgebreide XML-tutorial: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Erlang's `xmerl` documentatie voor XML-verwerking: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)
