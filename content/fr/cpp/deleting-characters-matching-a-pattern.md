---
title:    "C++: Suppression de caractères correspondant à un motif"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles un programmeur pourrait vouloir supprimer des caractères correspondant à un motif. Parfois, cela peut être nécessaire pour nettoyer des données importées ou pour traiter une chaîne de caractères complexe. Dans cet article, nous allons expliquer comment supprimer des caractères correspondant à un motif en utilisant le langage de programmation C++.

## Comment faire

Pour supprimer des caractères correspondant à un motif en C++, nous allons utiliser la bibliothèque `<regex>` qui offre un support pour les expressions rationnelles. Pour commencer, nous devons inclure cette bibliothèque dans notre code.

```C++
#include <iostream>
#include <string>
#include <regex>

using namespace std;
```

Ensuite, nous allons déclarer une chaîne de caractères et lui attribuer une valeur.

```C++
string maChaine = "Bonjour, ce texte contient des mots à supprimer #suppression";
```

Maintenant, nous allons définir notre motif à l'aide d'une expression rationnelle. Dans cet exemple, nous allons supprimer tous les mots qui commencent par le symbole "#" suivi de n'importe quel caractère.

```C++
regex motif("#\\w+");
```

Ensuite, nous allons utiliser la fonction `regex_replace()` pour remplacer les caractères correspondants à notre motif par une chaîne vide.

```C++
string resultat = regex_replace(maChaine, motif, "");
```

Enfin, nous allons afficher le résultat.

```C++
cout << resultat << endl;
```

Lorsque vous exécutez le code, vous devriez obtenir la chaîne de caractères suivante : "Bonjour, ce texte contient des mots à supprimer". Les mots qui commençaient par le symbole "#" ont été supprimés.

## Plongée en profondeur

Supprimer des caractères correspondant à un motif peut sembler simple, mais il est important de comprendre comment les expressions rationnelles fonctionnent pour utiliser correctement cette méthode. Une expression rationnelle est un motif de caractères qui décrit une série de chaînes de caractères. Dans notre exemple, nous avons utilisé "#\\w+" pour représenter tous les mots commençant par "#" suivis de n'importe quel caractère. Voici quelques caractères spéciaux couramment utilisés dans les expressions rationnelles :

- `.` : représente n'importe quel caractère
- `*` : représente 0 ou plusieurs occurrences du caractère précédent
- `+` : représente 1 ou plusieurs occurrences du caractère précédent
- `?` : représente 0 ou 1 occurrence du caractère précédent
- `[ ]` : représente un ensemble de caractères, par exemple `[a-z]` représente une lettre minuscule de a à z
- `^` : représente le début d'une chaîne
- `$` : représente la fin d'une chaîne

Pour en savoir plus sur les expressions rationnelles et les différents paramètres que vous pouvez utiliser avec `regex_replace()`, consultez la documentation de la bibliothèque `<regex>`.

## Voir aussi
- [Documentation de `<regex>` en français](https://www.cplusplus.com/reference/regex/)
- [Autres exemples d'utilisation de la bibliothèque `<regex>`](https://stackoverflow.com/questions/12552008/c-regex-replace-examples)