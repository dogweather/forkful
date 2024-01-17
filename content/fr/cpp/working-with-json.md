---
title:                "Travailler avec json"
html_title:           "C++: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-json.md"
---

{{< edit_this_page >}}

## Qu'est-ce que JSON et pourquoi les programmeurs l'utilisent-ils?

JSON (JavaScript Object Notation) est un format de données textuelles utilisé pour stocker et échanger des informations structurées entre différents systèmes informatiques. Les programmeurs l'utilisent souvent pour échanger des données entre des serveurs et des applications web ou mobiles. JSON est populaire en raison de sa simplicité, de sa lisibilité et de sa compatibilité avec de nombreux langages de programmation.

## Comment faire:

```C++
#include <iostream>
#include <jsoncpp/json/json.h> //include bibliothèque JSON

int main()
{
    //Création d'un objet JSON
    Json::Value obj;

    //Ajout de données à l'objet
    obj["nom"] = "Jean";
    obj["âge"] = 35;
    obj["adresse"] = "123 rue principale";

    //Conversion en chaîne JSON
    std::string json = obj.toStyledString();
    
    //Affichage du résultat
    std::cout << json << std::endl;

    return 0;
}

//Sortie:
//{"nom": "Jean", "âge": 35, "adresse": "123 rue principale"}
```

## Plongée en profondeur:

JSON a été créé en 2001 par Douglas Crockford et est basé sur la syntaxe de JavaScript. Il est largement utilisé dans les applications web car il est plus léger et plus facile à utiliser que le format XML. Les alternatives à JSON incluent XML, YAML et CSV, mais celles-ci sont souvent plus rigides ou plus complexes. En C++, la bibliothèque open-source [JsonCpp](https://github.com/open-source-parsers/jsoncpp) est souvent utilisée pour travailler avec JSON.

## Voir aussi:

- [Introduction à JSON](https://www.json.org/json-fr.html)
- [Documentation de JsonCpp](https://jsoncpp.sourceforge.io/)