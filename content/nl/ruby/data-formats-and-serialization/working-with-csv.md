---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:17.620295-07:00
description: 'Hoe te: **Een CSV lezen:**.'
lastmod: '2024-03-13T22:44:51.389241-06:00'
model: gpt-4-0125-preview
summary: '**Een CSV lezen:**.'
title: Werken met CSV
weight: 37
---

## Hoe te:
**Een CSV lezen:**

```Ruby
require 'csv'

CSV.foreach("pad/naar/bestand.csv", headers: true) do |rij|
  puts rij["HeaderNaam"] # Vervang door uw daadwerkelijke header
end
```

**Naar een CSV schrijven:**

```Ruby
require 'csv'

CSV.open("pad/naar/uitvoer.csv", "wb", write_headers: true, headers: ["Naam", "Leeftijd", "Stad"]) do |csv|
  csv << ["Alice", 32, "Wonderland"]
  csv << ["Bob", 46, "Springfield"]
end
```

**Voorbeelduitvoer:**

```Text
Alice, 32, Wonderland
Bob, 46, Springfield
```

## Diep Duiken
CSV bestaat al sinds de vroege dagen van computing en biedt een eenvoudige manier om tabelgegevens tussen programma's en systemen te verplaatsen. Alternatieven zijn onder andere JSON en XML, maar CSV blijft populair vanwege de eenvoud en lage overhead. Ruby's standaard CSV-bibliotheek, die gemakkelijk rond de onderliggende parsers is gewikkeld, biedt naadloze integratie, inclusief ondersteuning voor verschillende coderingen, aangepaste converters, en flexibele ontledingsopties.

## Zie Ook
- Ruby's CSV-bibliotheekdocumentatie: https://ruby-doc.org/stdlib-3.0.0/libdoc/csv/rdoc/CSV.html
- CSV op Wikipedia: https://nl.wikipedia.org/wiki/Comma-separated_values
- "FasterCSV" gem (oud maar relevant om historische redenen): https://rubygems.org/gems/fastercsv
