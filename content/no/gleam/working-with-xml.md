---
title:                "Å jobbe med XML"
date:                  2024-01-26T04:31:08.019570-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Arbeid med XML involverer parsing, manipulering og generering av XML-dokumenter, som brukes for datadeling på grunn av deres strukturerte og utbredte format. Programmerere håndterer XML for å grensesnitte med utallige systemer der XML er det felles språket for data.

## Hvordan:
Gleam støtter ikke XML nativt, så vi vil bruke et eksternt bibliotek som `gleam_xml`. Først, legg det til i din `gleam.toml`:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Nå, analyser og opprett XML:

```rust
import gleam/xml

// Parse XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Opprett XML
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

Eksempel på utdata for `xml.render(node)` er:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Dypdykk
XML står for eXtensible Markup Language, en spesifikasjon fra W3C som en søster til HTML. Det har vært rundt siden slutten av '90-tallet. For Gleam føles håndtering av XML litt som et skritt tilbake i tid. JSON og Protocol Buffers er trendigere, men XMLs omfattende bruk i arvesystemer og visse industrier betyr at det fremdeles er relevant.

Alternativer som `xmerl` finnes i Erlang-økosystemet; men, `gleam_xml` biblioteket tilbyr en mer idiomatisk tilnærming for Gleam-brukere. Det er bygget på toppen av eksisterende Erlang-biblioteker, men eksponerer et Gleam-vennlig API. Gleam sin tilnærming til XML sikter mot enkelhet og sikkerhet, reduserer unødvendig kode og legger vekt på typetrygghet.

Når det gjelder implementering, tilbyr XML-biblioteker inkludert `gleam_xml` vanligvis DOM-lignende strukturer. Dette innebærer noder, attributter og nestede elementer, og drar nytte av Erlangs mønstermatching og samtidighetsmodeller for å håndtere potensielt store og komplekse dokumenter.

## Se også
- `gleam_xml` biblioteket på Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Offisiell XML-standard av W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Omfattende XML-tutorial: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Erlangs `xmerl` dokumentasjon for XML-behandling: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)