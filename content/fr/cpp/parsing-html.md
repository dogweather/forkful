---
title:                "C++: Analyse de HTML"
simple_title:         "Analyse de HTML"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/parsing-html.md"
---

{{< edit_this_page >}}

Pourquoi: Nous savons tous que le langage HTML est la base de toutes les pages web. Mais saviez-vous qu'il était possible de "parser" (analyser) ce langage grâce à la programmation ? Dans cet article, nous allons vous expliquer pourquoi vous devriez vous intéresser à la parsing de HTML.

Comment faire: Tout d'abord, il est important de comprendre que le HTML est un langage balisé, composé de balises et de contenu à l'intérieur de ces balises. Le parsing de HTML consiste donc à analyser et à extraire les informations contenues dans ces balises pour ensuite les manipuler ou les afficher selon nos besoins.

En C++, il existe plusieurs bibliothèques qui nous permettent de parser du HTML de manière efficace, telles que Boost.PropertyTree, libxml2, ou encore RapidXML. Jetons un coup d'oeil à un exemple de code utilisant RapidXML :

```C++
#include <iostream>
#include <fstream>
#include "rapidxml.hpp"

using namespace rapidxml;

int main() {
    // Ouvrir et lire le fichier HTML
    std::ifstream file("exemple.html");
    std::stringstream buffer;
    buffer << file.rdbuf();
    file.close();

    // Convertir le contenu en chaîne de caractères
    std::string html = buffer.str();

    // Créer un document XML à partir de la chaîne de caractères
    xml_document<> doc;
    doc.parse<parse_declaration_node | parse_no_data_nodes>(&html[0]);

    // Parcourir les balises et afficher leur contenu
    for (xml_node<> *node = doc.first_node(); node; node = node->next_sibling()) {
        std::cout << node->name() << ": " << node->value() << std::endl;
    }
    return 0;
}
```

Output:

```
html:

head:

title:
Exemple de page web

/body:
```

Comme vous pouvez le voir dans cet exemple, nous avons pu parser le contenu du fichier HTML et accéder aux données des différentes balises grâce à la bibliothèque RapidXML.

Plongée en profondeur: Le parsing de HTML peut être utilisé dans de nombreux cas pratiques, tels que le scraping de données sur le web, la génération de rapports ou encore la création de moteurs de recherche. Il peut également être combiné avec d'autres techniques, comme l'utilisation d'expressions régulières, pour effectuer des manipulations plus complexes sur le contenu des balises.

Voyons un autre exemple de code, cette fois en utilisant la librairie Boost.PropertyTree, qui va nous permettre d'extraire uniquement le contenu des balises "p" d'une page web et de les stocker dans un vecteur :

```C++
#include <iostream>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/xml_parser.hpp>

using namespace boost::property_tree;

int main() {
    // Créer un arbre de propriétés à partir du fichier HTML
    ptree pt;
    read_xml("exemple.html", pt, xml_parser::trim_whitespace);

    // Parcourir les balises et stocker leur contenu dans un vecteur
    std::vector<std::string> paragraphs;
    for (auto &node : pt.get_child("html")) {
        if (node.first == "head") continue; // Ignorer le contenu de la balise "head"
        for (auto &child : node.second) {
            paragraphs.push_back(child.second.get_value<std::string>());
        }
    }

    // Afficher le contenu du vecteur
    for (const auto &p : paragraphs) {
        std::cout << p << std::endl;
    }

    return 0;
}
```

Output:

```
Exemple de paragraphe 1
Exemple de paragraphe 2
```

Comme vous pouvez le constater, la parsing de HTML peut se faire de différentes manières en fonction de vos besoins et de la bibliothèque utilisée.

Voir aussi: 
- https://www.w3schools.com/
- https://www.boost.org/doc/libs/1_65_1/doc/html/property_tree.html
- https://rapidxml.sourceforge.net/manual.html