---
title:                "Manipulation de JSON"
date:                  2024-01-19
html_title:           "Arduino: Manipulation de JSON"
simple_title:         "Manipulation de JSON"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Ce que c'est & Pourquoi ?

JSON, ou JavaScript Object Notation, est un format facile pour stocker et transporter des données. Les programmeurs l'utilisent souvent pour les échanges de données entre un serveur et une application web, parce que c'est léger, lisible par l'homme et facilement analysable par machines.

## Comment faire :

Pour manipuler du JSON en C++, on a besoin d'une librairie tierce comme `nlohmann::json`. Voici comment l'ajouter et l'utiliser :

```cpp
#include <nlohmann/json.hpp>
#include <iostream>

int main() {
    // Créer un objet JSON
    nlohmann::json obj;
    obj["prenom"] = "Jean";
    obj["age"] = 30;
    obj["passions"] = {"programmation", "musique", "café"};

    // Afficher l'objet JSON
    std::cout << obj << std::endl;

    // Parser un JSON depuis une string
    std::string json_string = R"({"ville":"Paris", "population":2148000})";
    nlohmann::json parsed = nlohmann::json::parse(json_string);

    // Accéder à une valeur
    std::cout << "La population de " << parsed["ville"] << " est de " << parsed["population"] << " habitants." << std::endl;
    
    return 0;
}
```

Sortie attendue :

```
{"age":30,"passions":["programmation","musique","café"],"prenom":"Jean"}
La population de Paris est de 2148000 habitants.
```

## Plongée profonde :

L'utilisation de JSON en C++ s'est répandue après l'adoption massive de JSON pour les APIs web. Auparavant, XML était le choix standard, mais sa complexité a souvent poussé à préférer JSON. Parmi les librairies C++ pour JSON, `nlohmann::json` sort du lot pour sa simplicité et sa conformité aux standards modernes. Elle imite la syntaxe de JavaScript pour une interaction familière. Côté performance, des alternatives comme `RapidJSON` ou `JsonCpp` peuvent être envisagées, surtout pour des applications critiques.

## Voir aussi :

- `nlohmann::json` GitHub page: [https://github.com/nlohmann/json](https://github.com/nlohmann/json)
- Documentation officielle JSON: [https://www.json.org/json-fr.html](https://www.json.org/json-fr.html)
- Comparatif de performances des librairies JSON en C++ : [https://github.com/miloyip/nativejson-benchmark](https://github.com/miloyip/nativejson-benchmark)
