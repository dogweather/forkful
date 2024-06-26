---
date: 2024-01-26 04:30:09.727398-07:00
description: "Comment faire : Fish n'a pas d'analyse XML int\xE9gr\xE9e, vous devrez\
  \ donc vous appuyer sur des outils externes comme `xmllint` ou `xmlstarlet`. Voici\
  \ un\u2026"
lastmod: '2024-03-13T22:44:58.350988-06:00'
model: gpt-4-0125-preview
summary: "Fish n'a pas d'analyse XML int\xE9gr\xE9e, vous devrez donc vous appuyer\
  \ sur des outils externes comme `xmllint` ou `xmlstarlet`."
title: Travailler avec XML
weight: 40
---

## Comment faire :
Fish n'a pas d'analyse XML intégrée, vous devrez donc vous appuyer sur des outils externes comme `xmllint` ou `xmlstarlet`. Voici un extrait pour lire des valeurs :

```fish
# Analyser l'XML à l'aide de xmlstarlet
echo '<root><element>Bonjour le monde</element></root>' | xmlstarlet sel -t -v "/root/element"
```

Sortie :
```
Bonjour le monde
```

Pour modifier l'XML, utilisez ceci :

```fish
# Modifier l'élément XML avec xmlstarlet
echo '<root><element>Ancienne Valeur</element></root>' | xmlstarlet ed -u "/root/element" -v 'Nouvelle Valeur'
```

Sortie :
```xml
<?xml version="1.0"?>
<root>
  <element>Nouvelle Valeur</element>
</root>
```

## Plongée Profonde :
L'XML existe depuis la fin des années 90, conçu pour la lisibilité et la convivialité machine. Bien que le JSON ait usurpé une partie de la popularité de l'XML en raison de sa simplicité, l'XML reste ancré là où la validation des documents et les espaces de noms sont clés.

Des alternatives ? Bien sûr - JSON, YAML, ou même des formats binaires comme les Protocol Buffers pour ces applications à forte intensité de performance. Mais le schéma XML et XSLT (pour les transformations XML) peuvent être des points de rupture pour des scénarios complexes où la robustesse importe.

Sous le capot, des outils comme `xmlstarlet` enveloppent des bibliothèques puissantes comme libxml2, vous offrant XPath et XQuery pour un bricolage XML de haute précision. Ce ne sont pas seulement des outils XML mais des passerelles vers la manipulation du DOM, comme vous appliqueriez des concepts similaires dans n'importe quel langage qui touche à l'XML.

## Voir Aussi :
- [Documentation xmlstarlet](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Documentation de Fish](https://fishshell.com/docs/current/index.html)
- [Fonctions et Opérateurs XPath et XQuery](https://www.w3.org/TR/xpath-functions/)
