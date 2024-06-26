---
date: 2024-01-26 04:29:40.040450-07:00
description: "Comment faire : Elixir n'inclut pas l'analyse XML dans sa biblioth\xE8\
  que standard. SweetXML est un choix populaire. Voici comment l'utiliser ."
lastmod: '2024-03-13T22:44:57.353976-06:00'
model: gpt-4-0125-preview
summary: "Elixir n'inclut pas l'analyse XML dans sa biblioth\xE8que standard."
title: Travailler avec XML
weight: 40
---

## Comment faire :
Elixir n'inclut pas l'analyse XML dans sa bibliothèque standard. SweetXML est un choix populaire. Voici comment l'utiliser :

```elixir
# Ajoutez SweetXML à vos dépendances dans mix.exs
{:sweet_xml, "~> 0.6"}

# Dans votre code
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Rappel</heading>
  <body>Ne m'oublie pas ce week-end !</body>
</note>
"""

# Parser XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # Sortie : Tove
```

## Plongée Profonde
XML, ou Extensible Markup Language, existe depuis la fin des années 90. Il est verbeux mais structuré—idéal pour l'échange de données complexes. Alors que la popularité de JSON a grimpé en flèche pour sa simplicité, XML reste ancré dans de nombreux systèmes d'entreprise et financiers pour son expressivité et ses schémas standardisés.

Les alternatives incluent :
- JSON pour un échange de données moins verbeux et plus léger.
- Protobuf ou Thrift pour la communication de données sérialisées en binaire, particulièrement pour les systèmes internes.

Sous le capot, les bibliothèques XML pour Elixir tirent parti de la bibliothèque :xmerl d'Erlang pour l'analyse, qui offre un support robuste mais peut être moins intuitive que des approches plus modernes. Au fur et à mesure que Elixir évolue, les bibliothèques pilotées par la communauté comme SweetXML enveloppent celles-ci avec une syntaxe plus Elixir-esque, rendant les manipulations XML plus accessibles.

## Voir Aussi :
- SweetXML sur Hex : https://hex.pm/packages/sweet_xml
- L'approche d'Elixir pour l'analyse XML : https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- Documentation xmerl pour la gestion sous-jacente de XML : http://erlang.org/doc/apps/xmerl/index.html
