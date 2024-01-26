---
title:                "Att arbeta med XML"
date:                  2024-01-26T04:31:11.036306-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att arbeta med XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att arbeta med XML involverar tolkning, manipulering och generering av XML-dokument, vilka används för datautbyte på grund av deras strukturerade och utbredda format. Programmerare hanterar XML för att interagera med otaliga system där XML är lingua franca för data.

## Hur:
Gleam har inte inbyggt stöd för XML, så vi kommer att använda ett externt bibliotek som `gleam_xml`. Lägg först till det i din `gleam.toml`:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Nu, tolka och skapa XML:

```rust
import gleam/xml

// Tolk XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Skapa XML
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

Exempelutdata för `xml.render(node)` är:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Fördjupning
XML står för eXtensible Markup Language, en specifikation från W3C som en syster till HTML. Den har funnits sedan slutet av 90-talet. För Gleam känns hanteringen av XML lite som att ta ett steg tillbaka i tiden. JSON och Protocol Buffers är trendigare, men XML:s omfattande användning i äldre system och vissa industrier innebär att det fortfarande är relevant.

Alternativ som `xmerl` finns i Erlang-ekosystemet; dock erbjuder `gleam_xml`-biblioteket ett mer idiomatiskt tillvägagångssätt för Gleam-användare. Det är byggt på befintliga Erlang-bibliotek men exponerar ett API som är vänligt för Gleam. Gleams tillvägagångssätt för XML siktar på enkelhet och säkerhet, reducerar mallkoden och betonar typsäkerhet.

Ur ett implementeringsperspektiv erbjuder XML-bibliotek inklusive `gleam_xml` typiskt DOM-liknande strukturer. Detta innefattar noder, attribut och nästlade element, och utnyttjar Erlangs mönstermatchning och konkurrensmodeller för att hantera potentiellt stora och komplexa dokument.

## Se även
- `gleam_xml`-biblioteket på Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Officiella XML-standarden av W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Omfattande XML-tutorial: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Erlangs `xmerl`-dokumentation för XML-behandling: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)