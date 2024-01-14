---
title:                "C++: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur en herbe ou même expérimenté, vous avez probablement déjà entendu parler de l'extraction de sous-chaînes. L'extraction de sous-chaînes est une tâche courante en programmation qui consiste à extraire une partie spécifique d'une chaîne de caractères. Vous pourriez vous demander pourquoi vous devriez vous préoccuper de cette tâche apparemment simple. La réponse est simple : l'extraction de sous-chaînes est une compétence clé pour manipuler des données textuelles et peut être utilisée pour une variété d'applications, telles que le traitement de texte, le traitement de données et la recherche d'informations.

## Comment faire

Pour extraire une sous-chaîne en C++, vous pouvez utiliser la méthode "substr" de la classe "string". Cette méthode prend deux paramètres : l'index de début et le nombre de caractères à extraire. Par exemple, si vous avez une chaîne "Bonjour" et que vous voulez extraire la sous-chaîne "onj", vous pouvez utiliser la méthode de cette manière :

```C++
string str = "Bonjour";
string sub = str.substr(1, 3); 
cout << sub; // affiche "onj"
```

Dans cet exemple, nous avons utilisé l'index "1" comme point de départ pour extraire la sous-chaîne et le nombre "3" pour spécifier la quantité de caractères à extraire.

## Plongée en profondeur

Si vous voulez en savoir plus sur l'extraction de sous-chaînes en C++, il est important de comprendre l'indexation des chaînes de caractères. En C++, les indices commencent à 0, ce qui signifie que le premier caractère d'une chaîne a l'index 0, le deuxième a l'index 1, et ainsi de suite. En utilisant cette notation, vous pouvez spécifier facilement les indices de début et de fin pour extraire la sous-chaîne souhaitée.

De plus, vous pouvez également spécifier un seul paramètre dans la méthode "substr" pour extraire tout le reste de la chaîne à partir de l'index donné jusqu'à la fin de la chaîne. Par exemple, en utilisant l'indice 2 dans notre exemple précédent, la sous-chaîne extraite serait "njour".

## Voir aussi

Si vous voulez en savoir plus sur l'extraction de sous-chaînes en C++, voici quelques liens utiles :

- Tutoriel sur les chaînes de caractères en C++ : www.cplusplus.com/doc/tutorial/strings/
- Documentation de la méthode "substr" : www.cplusplus.com/reference/string/string/substr/
- Exemples de l'utilisation de l'extraction de sous-chaînes en C++ : github.com/search?q=substring+in+C%2B%2B