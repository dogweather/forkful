---
title:    "C++: Suppression de caractères correspondant à un modèle"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif est une tâche courante dans la programmation, en particulier lors de la manipulation de chaînes de caractères. Cela peut être utile pour supprimer des caractères spécifiques tels que les espaces ou les balises HTML d'une chaîne de texte. Dans cet article, nous allons plonger dans cette problématique et explorer différentes façons de l'implémenter en C++.

## Comment Faire

Il existe plusieurs façons de supprimer des caractères correspondant à un motif en C++. Le plus simple consiste à utiliser la méthode `erase` de la classe `string` pour supprimer tous les caractères correspondant au motif. Voici un exemple de code :

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Bonjour tout le monde!";
    
    // Supprime tous les espaces
    str.erase(std::remove(str.begin(), str.end(), ' '), str.end());
    
    std::cout << str << std::endl; // affiche "Bonnejourtoutlemonde!"
    return 0;
}
```

Dans cet exemple, nous utilisons également la fonction `remove` de la bibliothèque standard `<algorithm>` pour trouver tous les espaces dans la chaîne de caractères et les supprimer à l'aide de la méthode `erase`.

Une autre méthode consiste à utiliser des expressions régulières (regex) pour trouver et supprimer les caractères correspondant au motif. Voici un exemple de code utilisant la bibliothèque `<regex>` :

```C++
#include <iostream>
#include <regex>
#include <string>

int main() {
    std::string str = "Hello <strong>World!</strong>";
    
    // Supprime toutes les balises HTML
    str = std::regex_replace(str, std::regex("<[^>]*>"), "");
    
    std::cout << str << std::endl; // affiche "Hello World!"
    return 0;
}
```

Dans cet exemple, nous utilisons la fonction `regex_replace` pour remplacer toutes les balises HTML (délimitées par les symboles < et >) par une chaîne vide.

## Plongée Profonde

En supprimant des caractères correspondant à un motif, il est important de comprendre comment se comporte la méthode `erase`. Elle prend en paramètre une plage de caractères à supprimer, définie par un itérateur pointant vers le premier caractère à supprimer et un itérateur pointant vers le dernier caractère à supprimer. Cela signifie qu'il est possible de supprimer une seule occurrence d'un caractère ou une plage de caractères. De plus, la méthode `erase` peut également être utilisée avec des itérateurs de chaînes de caractères pour supprimer des sous-chaînes.

En utilisant des expressions régulières, il est important de savoir que toutes les fonctions de la bibliothèque `<regex>` ne sont pas compatibles avec tous les compilateurs. Il est recommandé de vérifier la compatibilité avant d'utiliser des expressions régulières dans votre code.

## Voir Aussi

- [Documentation de la méthode `erase`](https://www.cplusplus.com/reference/string/string/erase/)
- [Guide des expressions régulières en C++](https://www.tutorialspoint.com/cpp_std_lib/regular_expressions.htm)
- [Compatibilité des expressions régulières selon le compilateur](http://www.cplusplus.com/reference/regex/ECMAScript/)