---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:15.639804-07:00
description: 'Hoe: Elixir bevat geen XML-parsing in zijn standaardbibliotheek. SweetXML
  is een populaire keuze. Hier is hoe je het gebruikt.'
lastmod: '2024-03-13T22:44:50.486081-06:00'
model: gpt-4-0125-preview
summary: Elixir bevat geen XML-parsing in zijn standaardbibliotheek.
title: Werken met XML
weight: 40
---

## Hoe:
Elixir bevat geen XML-parsing in zijn standaardbibliotheek. SweetXML is een populaire keuze. Hier is hoe je het gebruikt:

```elixir
# Voeg SweetXML toe aan je afhankelijkheden in mix.exs
{:sweet_xml, "~> 0.6"}

# In je code
importeer SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Herinnering</heading>
  <body>Vergeet me dit weekend niet!</body>
</note>
"""

# Parseer XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # Uitvoer: Tove
```

## Diepere duik
XML, of Extensible Markup Language, bestaat al sinds eind jaren '90. Het is langdradig maar gestructureerd—ideaal voor complexe gegevensuitwisseling. Terwijl de populariteit van JSON steeg vanwege zijn eenvoud, blijft XML verankerd in veel enterprise- en financiële systemen vanwege zijn expressiviteit en gestandaardiseerde schema's.

Alternatieven zijn onder andere:
- JSON voor lichtere, minder langdradige gegevensuitwisseling.
- Protobuf of Thrift voor binaire geserialiseerde gegevenscommunicatie, in het bijzonder voor interne systemen.

Onder de motorkap gebruiken XML-bibliotheken voor Elixir Erlang's :xmerl bibliotheek voor parsing, wat robuuste ondersteuning biedt maar minder intuïtief kan zijn dan modernere benaderingen. Naarmate Elixir evolueert, wikkelen door de gemeenschap gestuurde bibliotheken zoals SweetXML deze in met een meer Elixir-achtige syntax, waardoor XML-manipulaties toegankelijker worden.

## Zie ook:
- SweetXML op Hex: https://hex.pm/packages/sweet_xml
- Elixir's visie op XML-parsing: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- xmerl documentatie voor onderliggende XML-afhandeling: http://erlang.org/doc/apps/xmerl/index.html
