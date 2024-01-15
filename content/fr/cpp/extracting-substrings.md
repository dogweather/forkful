---
title:                "Extraction de sous-chaînes"
html_title:           "C++: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Pourquoi extrait-on des sous-chaînes en programmation C++ ?

Extrait les sous-chaînes est utile lorsque l'on souhaite manipuler ou analyser une partie spécifique d'une chaîne de caractères plus longue.

## Comment faire ?

```C++
// Déclaration d'une chaîne de caractères
string chaine = "Hello World!";

// Extrait les 5 premiers caractères de la chaîne:
string sous_chaine = chaine.substr(0, 5);

// Affiche la sous-chaîne extraite
cout << sous_chaine << endl;

// Output : Hello
```

Les sous-chaînes sont extraites à l'aide de la méthode `substr()` de la classe `string`. Cette méthode prend deux paramètres : l'index de départ et la longueur de la sous-chaîne à extraire. En utilisant cette méthode, vous pouvez facilement manipuler et traiter des sous-parties de vos chaînes de caractères.

## Plongée en profondeur

La méthode `substr()` retourne une nouvelle chaîne de caractères contenant la sous-chaîne extraite, sans modifier la chaîne d'origine. Vous pouvez également utiliser des indices négatifs pour spécifier le nombre de caractères à partir de la fin de la chaîne. Par exemple, `chaine.substr(-3)` va extraire les 3 derniers caractères de la chaîne.

# Voir aussi
- [Documentation officielle de la classe string en C++](https://docs.microsoft.com/fr-fr/cpp/standard-library/string-class?view=msvc-160)
- [Explications sur la manipulation de chaînes de caractères en C++](https://www.developpez.net/forums/d1988378/cpp/general-cpp/traitement-chaines-caracteres/#post9864081)