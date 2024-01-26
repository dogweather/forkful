---
title:                "Travailler avec XML"
date:                  2024-01-26T04:30:59.420805-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec XML implique l'analyse, la manipulation et la génération de documents XML, qui sont utilisés pour l'échange de données en raison de leur format structuré et répandu. Les programmeurs manipulent l'XML pour interagir avec d'innombrables systèmes où l'XML est la lingua franca des données.

## Comment faire :
Gleam ne prend pas en charge nativement l'XML, donc nous utiliserons une bibliothèque externe comme `gleam_xml`. Tout d'abord, ajoutez-la à votre `gleam.toml` :

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Maintenant, analysez et créez du XML :

```rust
import gleam/xml

// Analyser le XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Créer du XML
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

Exemple de sortie pour `xml.render(node)` :

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Plongée profonde
XML signifie eXtensible Markup Language, une spécification du W3C comme une sœur du HTML. Il existe depuis la fin des années 90. Pour Gleam, manipuler l'XML donne un peu l'impression de faire un pas en arrière dans le temps. Le JSON et les Protocoles Buffers sont plus tendance, mais l'utilisation étendue de l'XML dans les systèmes hérités et certaines industries signifie qu'il est toujours pertinent.

Des alternatives comme `xmerl` existent dans l'écosystème Erlang ; cependant, la bibliothèque `gleam_xml` propose une approche plus idiomatique pour les utilisateurs de Gleam. Elle est construite sur la base de bibliothèques Erlang existantes mais expose une API conviviale pour Gleam. L'approche de Gleam envers l'XML vise la simplicité et la sécurité, en réduisant le code standard et en mettant l'accent sur la sécurité des types.

En termes d'implémentation, les bibliothèques XML, y compris `gleam_xml`, fournissent généralement des structures de type DOM. Cela implique des nœuds, des attributs et des éléments imbriqués, en exploitant le modèle de correspondance de motif et de concurrence d'Erlang pour gérer des documents potentiellement volumineux et complexes.

## Voir également
- La bibliothèque `gleam_xml` sur Hex : [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- La norme officielle XML par le W3C : [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Tutoriel XML complet : [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Documentation d'`xmerl` d'Erlang pour le traitement XML : [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)