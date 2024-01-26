---
title:                "XML:n käsittely"
date:                  2024-01-26T04:31:11.261088-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML:n käsittely"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Työskentely XML:n parissa sisältää XML-dokumenttien jäsentämisen, muokkaamisen ja luomisen. XML-dokumentteja käytetään datan vaihtoon niiden rakenteellisen ja laajalle levinneen muodon vuoksi. Ohjelmoijat käsittelevät XML:ää liittyäkseen lukemattomiin järjestelmiin, joissa XML on datan lingua franca.

## Kuinka:
Gleam ei tue XML:ää natiivisti, joten käytämme ulkoista kirjastoa kuten `gleam_xml`. Lisää se ensin `gleam.toml`-tiedostoosi:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Nyt, jäsentäkää ja luokaa XML:

```rust
import gleam/xml

// Jäsennä XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Luo XML
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

Esimerkkituloste `xml.render(node)`-komennolle on:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Syväsukellus
XML tarkoittaa eXtensible Markup Language -merkintäkieltä, joka on W3C:n spesifikaatio ja HTML:n sisar. Se on ollut olemassa 90-luvun lopulta. Gleamin käsitellessä XML:ää, se tuntuu hieman kuin askeleelta taaksepäin ajassa. JSON ja Protocol Buffers ovat trendikkäämpiä, mutta XML:n laaja käyttö legacy-järjestelmissä ja tietyillä teollisuudenaloilla tarkoittaa, että se on edelleen relevantti.

Vaihtoehtoja kuten `xmerl` on olemassa Erlangin ekosysteemissä; kuitenkin `gleam_xml`-kirjasto tarjoaa idiomaattisemman lähestymistavan Gleam-käyttäjille. Se on rakennettu olemassaolevien Erlang-kirjastojen päälle, mutta tarjoaa Gleam-ystävällisen API:n. Gleamin lähestymistapa XML:ään pyrkii yksinkertaisuuteen ja turvallisuuteen, vähentäen boilerplate-koodia ja korostaen tyypin turvallisuutta.

Toteutuksen näkökulmasta, XML-kirjastot mukaan lukien `gleam_xml` tarjoavat tyypillisesti DOM-kaltaisia rakenteita. Tämä sisältää solmut, attribuutit ja sisäkkäiset elementit, hyödyntäen Erlangin kuviohakua ja samanaikaisuusmalleja käsitelläkseen potentiaalisesti suuria ja monimutkaisia dokumentteja.

## Katso myös
- `gleam_xml`-kirjasto Hex:ssä: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Virallinen XML-standardi W3C:ssä: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Kattava XML-opas: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Erlangin `xmerl`-dokumentaatio XML:n käsittelyyn: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)