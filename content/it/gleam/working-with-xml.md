---
title:                "Lavorare con XML"
date:                  2024-01-26T04:30:59.244097-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lavorare con XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Lavorare con XML implica il parsing, la manipolazione e la generazione di documenti XML, che vengono utilizzati per lo scambio di dati a causa del loro formato strutturato e diffuso. I programmatori gestiscono XML per interfacciarsi con innumerevoli sistemi in cui XML è la lingua franca dei dati.

## Come fare:
Gleam non supporta nativamente XML, quindi utilizzeremo una libreria esterna come `gleam_xml`. Prima, aggiungila al tuo `gleam.toml`:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Ora, analizza e crea XML:

```rust
import gleam/xml

// Analizza XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Crea XML
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

L'output di esempio per `xml.render(node)` è:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Approfondimento
XML sta per eXtensible Markup Language, una specifica del W3C come sorella di HTML. Esiste dalla fine degli anni '90. Per Gleam, gestire XML sembra un po' come fare un passo indietro nel tempo. JSON e Protocol Buffers sono più alla moda, ma l'uso esteso di XML in sistemi legacy e in certe industrie significa che è ancora rilevante.

Alternative come `xmerl` esistono nell'ecosistema di Erlang; tuttavia, la libreria `gleam_xml` offre un approccio più idiomatico per gli utenti di Gleam. È costruita sopra le librerie Erlang esistenti ma espone un'API amichevole per Gleam. L'approccio di Gleam all'XML mira alla semplicità e alla sicurezza, riducendo il codice boilerplate e enfatizzando la sicurezza dei tipi.

Dal punto di vista dell'implementazione, le librerie XML inclusa `gleam_xml` tipicamente forniscono strutture simili al DOM. Questo comporta nodi, attributi ed elementi nidificati, sfruttando il pattern matching di Erlang e i modelli di concorrenza per gestire documenti potenzialmente grandi e complessi.

## Vedi Anche
- La libreria `gleam_xml` su Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Lo standard XML ufficiale del W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Tutorial completo su XML: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Documentazione di `xmerl` di Erlang per l'elaborazione XML: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)