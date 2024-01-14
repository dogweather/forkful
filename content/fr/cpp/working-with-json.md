---
title:                "C++: Travailler avec json"
simple_title:         "Travailler avec json"
programming_language: "C++"
category:             "C++"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/working-with-json.md"
---

{{< edit_this_page >}}

# Pourquoi travailler avec JSON?
Si vous êtes un développeur débutant en C++ ou si vous êtes habitué à travailler avec des structures de données plus traditionnelles telles que des tableaux ou des objets, vous vous demandez peut-être pourquoi vous devriez vous intéresser à JSON. La réponse est simple : JSON est un format léger et facile à utiliser pour échanger et stocker des données, ce qui en fait un choix populaire dans le monde de la programmation.

# Comment travailler avec JSON en C++?
Pour pouvoir utiliser JSON dans vos projets C++, vous devez d'abord inclure la bibliothèque de traitement JSON. Ensuite, vous pouvez créer un objet JSON en utilisant la classe "json::value", qui vous permet de stocker des données de différents types (chaînes de caractères, nombres, tableaux, objets). Voici un exemple de code pour créer un objet JSON contenant des informations sur un utilisateur :

```C++
#include <iostream>
#include <cpprest/json.h> // bibliothèque de traitement JSON

using namespace std;
using namespace web::json; // utilisation de la classe json::value

int main()
{
    // création de l'objet JSON
    json::value user;
    user["nom"] = json::value::string("Jean");
    user["age"] = json::value::number(25);
    user["hobbies"] = json::value::array({ json::value::string("lecture"), json::value::string("voyages")});
    user["adresse"] = json::value::object({
        {U("rue"), json::value::string("Rue de la Liberté")},
        {U("ville"), json::value::string("Paris")}
    });

    // affichage des données de l'utilisateur
    cout << "Nom : " << user["nom"].as_string() << endl;
    cout << "Age : " << user["age"].as_number() << endl;
    cout << "Hobbies : ";
    auto hobbies = user["hobbies"].as_array();
    for (auto it = hobbies.begin(); it != hobbies.end(); ++it) {
        cout << it->as_string() << " ";
    }
    cout << endl;
    cout << "Adresse : " << user["adresse"]["rue"].as_string() << ", " << user["adresse"]["ville"].as_string() << endl;

    return 0;
}
```

Voici le résultat obtenu :

```
Nom : Jean
Age : 25
Hobbies : lecture voyages
Adresse : Rue de la Liberté, Paris
```

# Plongée dans le monde de JSON
JSON permet également de convertir facilement des données en ligne de commande en objets JSON, grâce à la classe "json::value::parse". De plus, la bibliothèque de traitement JSON propose de nombreuses autres fonctionnalités telles que la création de requêtes HTTP avec des objets JSON en tant que corps de la requête, ou encore la validation de la structure de vos données JSON. N'hésitez pas à consulter la documentation pour en savoir plus sur ces fonctionnalités et sur la manière d'utiliser JSON de manière optimale dans vos projets C++.

# Voir aussi
- [Documentation officielle de la bibliothèque de traitement JSON en C++](https://microsoft.github.io/cpprestsdk)
- [Tutoriel sur l'utilisation de JSON en C++](https://www.geeksforgeeks.org/json-in-cpp/)
- [Introduction à JSON en C++](https://www.pluralsight.com/guides/parse-json-cpp)