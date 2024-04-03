---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:40.509119-07:00
description: "Een string omzetten naar kleine letters betekent dat alle letters binnen\
  \ een string worden veranderd in hun kleine lettervorm. Programmeurs doen dit voor\u2026"
lastmod: '2024-03-13T22:44:50.449126-06:00'
model: gpt-4-0125-preview
summary: Een string omzetten naar kleine letters betekent dat alle letters binnen
  een string worden veranderd in hun kleine lettervorm.
title: Een string omzetten naar kleine letters
weight: 4
---

## Wat & Waarom?

Een string omzetten naar kleine letters betekent dat alle letters binnen een string worden veranderd in hun kleine lettervorm. Programmeurs doen dit voor consistentie in gegevensopslag, vergelijkingen en zoekopdrachten.

## Hoe te:

Elixir maakt het een fluitje van een cent. Gebruik de `String.downcase/1` functie:

```elixir
origineel = "LoReM IPSUM"
klein = String.downcase(origineel)

IO.puts origineel
IO.puts klein
```

Uitvoer:

```
LoReM IPSUM
lorem ipsum
```

## Diepere Duik

Elixirs stringverwerking is zich bewust van Unicode, wat erg belangrijk is voor correcte omzetting naar kleine letters in verschillende alfabetten en schriftsystemen. Historisch gezien, hielden programmeertalen niet altijd rekening met deze complexiteit bij stringmanipulatie. 

Voordat Elixir deze huidige aanpak had, boden sommige oudere talen simplistische methoden die prima zouden kunnen werken voor Engels, maar zouden struikelen over talen zoals het Turks, waar bijvoorbeeld een hoofdletter 'İ' niet wordt 'I' maar 'İ'.

Intern gebruikt Elixir Unicode's kaartvorming voor het omzetten van kasten om dit correct te krijgen. En er zijn alternatieven; bijvoorbeeld, `String.downcase/2` laat je een taalinstelling specificeren, wat handig is voor taalspecifiek gedrag.

```elixir
turks = "GÖLCÜK"
String.downcase(turks, :tr)
```

Uitvoer:

```
gölcük
```

In het bovenstaande voorbeeld, let op hoe het 'İ'-karakter passend wordt behouden volgens de Turkse omzettingsregels.

## Zie Ook

- Elixirs officiële documentatie van het `String`-module: https://hexdocs.pm/elixir/String.html
- Unicode-kaartvorming voor kasten: https://www.unicode.org/reports/tr21/tr21-5.html
- Een snelle gids voor Unicode in Elixir: https://elixir-lang.org/blog/2017/01/05/elixir-and-unicode-part-1/
