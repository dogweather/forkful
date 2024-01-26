---
title:                "XML:n käsittely"
date:                  2024-01-26T04:29:53.369643-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML:n käsittely"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/working-with-xml.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
XML:n käsittely Elixirissä tarkoittaa XML-tietojen jäsentämistä, luomista ja manipulointia. Ohjelmoijat käsittelevät XML:ää, koska se on laajalti käytössä web-palveluissa, konfiguraatiotiedostoissa ja vanhoissa järjestelmissä.

## Kuinka:
Elixir ei sisällä XML-jäsentämistä vakio kirjastossaan. SweetXML on suosittu valinta. Näin sitä käytetään:

```elixir
# Lisää SweetXML riippuvuuksiin mix.exs-tiedostossa
{:sweet_xml, "~> 0.6"}

# Koodissasi
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Muistutus</heading>
  <body>Älä unohda minua tänä viikonloppuna!</body>
</note>
"""

# Jäsennä XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # Tuloste: Tove
```

## Syväsukellus
XML eli Laajennettava Merkintäkieli on ollut olemassa 90-luvun lopusta lähtien. Se on verbositetiivinen, mutta rakenteellinen - ideaali monimutkaisille tiedonvaihdoille. Vaikka JSONin suosio nousi sen yksinkertaisuuden vuoksi, XML pysyy vakiintuneena monissa yritys- ja finanssijärjestelmissä sen ilmaisuvoiman ja standardoitujen kaavioiden vuoksi.

Vaihtoehtoja ovat:
- JSON kevyemmälle, vähemmän verbositetiiviselle tiedonvaihdolle.
- Protobuf tai Thrift binääriserialisoituja dataviestinnälle, erityisesti sisäisissä järjestelmissä.

Pinnan alla Elixirin XML-kirjastot hyödyntävät Erlangin :xmerl-kirjastoa jäsentämiseen, joka tarjoaa vankkaa tukea, mutta voi olla vähemmän intuitiivinen kuin modernimmat lähestymistavat. Elixirin kehittyessä yhteisön ohjaamat kirjastot kuten SweetXML kietovat nämä Elixir-mielisemmällä syntaksilla, tehden XML-manipulaatiot helpommin lähestyttäviksi.

## Katso myös:
- SweetXML Hexissä: https://hex.pm/packages/sweet_xml
- Elixiring katsaus XML-jäsennykseen: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- xmerl-dokumentaatio taustalla olevasta XML-käsittelystä: http://erlang.org/doc/apps/xmerl/index.html