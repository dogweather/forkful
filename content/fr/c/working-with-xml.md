---
title:                "Travailler avec XML"
date:                  2024-01-26T04:27:58.025703-07:00
model:                 gpt-4-0125-preview
simple_title:         "Travailler avec XML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-xml.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Travailler avec XML en C comprend l'analyse, la création et la manipulation de fichiers XML - essentiellement un stockage de données structuré. Les programmeurs font cela pour interagir avec des données dans un format portable et lisible par l'homme, souvent utilisé pour la configuration, l'échange de données, et plus encore.

## Comment faire :
Ci-dessous se trouve un extrait utilisant la bibliothèque `libxml2` pour analyser un fichier XML et récupérer l'élément racine.

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *element_racine = NULL;

    // Analyser le fichier XML
    doc = xmlReadFile("exemple.xml", NULL, 0);

    // Obtenir l'élément racine
    element_racine = xmlDocGetRootElement(doc);

    printf("Élément racine : %s\n", element_racine->name);

    // Libérer le document
    xmlFreeDoc(doc);

    // Nettoyer l'analyseur
    xmlCleanupParser();

    return 0;
}
```

Un exemple de sortie pour un XML avec racine `<data>` pourrait être :
```
Élément racine : data
```

## Plongée Profonde
XML, ou Extensible Markup Language, remonte à la fin des années 90, fournissant un moyen de décrire et de structurer les données. En C, `libxml2` est la référence. Il est robuste, bien qu'il ne soit pas le plus facile pour les débutants en XML. Les alternatives incluent `tinyxml2`, qui est plus léger et plus adapté aux débutants. En ce qui concerne l'implémentation, C n'a pas de support XML intégré, donc les bibliothèques comblent cette lacune. Elles varient en taille, vitesse, complexité et portabilité. La plupart offrent des méthodes de parsing DOM et SAX : DOM charge la totalité dans la mémoire, bien pour les petits documents ; SAX est piloté par événements, traitant les éléments à la volée, mieux pour les gros fichiers. Les deux ont leurs cas d'utilisation et compromis.

## Voir Aussi
- [libxml2](http://xmlsoft.org/)
- [tinyxml2 sur GitHub](https://github.com/leethomason/tinyxml2)
- [Tutoriel XML sur w3schools](https://www.w3schools.com/xml/)
- [Spécification XML par W3C](https://www.w3.org/XML/)
