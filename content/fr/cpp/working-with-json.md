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

## Pourquoi

Si vous travaillez en C++, il est fort probable que vous aurez à manipuler des données au format JSON à un moment donné. JSON, ou JavaScript Object Notation, est un format de données très populaire pour le transfert et le stockage de données structurées. Il est donc important de connaître les bases de la manipulation de JSON en C++ pour travailler efficacement avec des données externes.

## Comment faire

Pour utiliser JSON en C++, vous aurez besoin d'une bibliothèque de JSON. Heureusement, il existe plusieurs options pour cela, dont certaines sont même intégrées à la bibliothèque standard du langage C++.

Nous allons vous montrer comment utiliser la bibliothèque JSON pour C++ développée par NeOKH dans ce tutoriel. Tout d'abord, assurez-vous d'avoir téléchargé et installé la bibliothèque. Ensuite, importez la bibliothèque dans votre projet avec l'instruction `#include "json.hpp"`.

```C++
#include "json.hpp"
```

Ensuite, vous pouvez créer un objet JSON en utilisant la classe `json` de la bibliothèque.

```C++
json monObjet = {
    { "nom", "Marie" },
    { "âge", 28 },
    { "langages", { "C++", "Python", "JavaScript" } }
};
```

Vous pouvez également lire un fichier JSON externe en utilisant la fonction `parse()`.

```C++
json monObjet = nlohmann::json::parse(fichier.json);
```

Pour accéder aux données d'un objet JSON, vous pouvez utiliser des opérateurs tels que `[]` et `.at()`, ainsi que des fonctions telles que `find()`.

```C++
cout << monObjet["nom"] << endl; // affiche "Marie"
cout << monObjet.at("âge") << endl; // affiche 28
```

Vous pouvez également ajouter ou modifier des données dans un objet JSON en utilisant les mêmes opérateurs et fonctions. L'objet sera automatiquement mis à jour avec les nouvelles données.

```C++
monObjet["ville"] = "Paris"; // ajoute une nouvelle clé/valeur à l'objet
monObjet.at("âge") = 29; // met à jour la valeur de la clé existante
```

Pour enregistrer un objet JSON dans un fichier, vous pouvez utiliser la fonction `dump()`.

```C++
ofstream fichierSortie("fichier.json");
fichierSortie << monObjet.dump(4);
```

La fonction `dump()` peut également accepter un paramètre pour formater l'objet JSON avec un nombre spécifique d'indentations.

## Plongée en profondeur

La bibliothèque JSON pour C++ est extrêmement flexible et offre de nombreuses fonctionnalités utiles pour la manipulation de données. Vous pouvez trouver plus d'informations dans la documentation officielle de la bibliothèque, ainsi que dans des tutoriels et des exemples en ligne.

Une autre fonctionnalité intéressante de la bibliothèque est la possibilité de valider un objet JSON pour s'assurer qu'il est bien formaté avant de le manipuler. Cela peut être fait en utilisant la fonction `is_valid()`.

```C++
if (monObjet.is_valid()) {
    // votre code ici
}
else {
    cout << "L'objet JSON est mal formaté" << endl;
}
```

## Voir aussi

- [Documentation officielle de la bibliothèque JSON pour C++](https://github.com/nlohmann/json)
- [Tutoriel vidéo sur la manipulation de JSON en C++](https://www.youtube.com/watch?v=9egd0IY8mRU)
- [Exemples pratiques de la bibliothèque JSON en C++](https://github.com/nlohmann/json/tree/develop/examples)