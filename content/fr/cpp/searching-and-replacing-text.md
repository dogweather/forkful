---
title:                "C++: Rechercher et remplacer du texte"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi
Vous vous êtes déjà retrouvé en train de chercher et remplacer du texte dans votre code C++ ? Cela peut sembler fastidieux, mais c'est en réalité une étape importante dans le processus de développement. En utilisant des techniques appropriées, vous pouvez gagner du temps et éviter d'introduire des erreurs dans votre code.

## Comment faire
Voici un exemple simple qui montre comment effectuer une recherche et un remplacement de texte en utilisant la fonction `find_and_replace` en C++ :

````C++
#include <iostream>
#include <string>

void find_and_replace(std::string& str, const std::string& search, const std::string& replace) {
    size_t start = 0;

    while ((start = str.find(search, start)) != std::string::npos) {
        str.replace(start, search.length(), replace);
        start += replace.length();
    }
}

int main() {
    std::string str = "Bonjour le monde!";
    std::string search = "le";
    std::string replace = "la";

    find_and_replace(str, search, replace);
    std::cout << str;

    return 0;
}
````

La sortie sera : "Bonjour la monde!"

Dans cet exemple, nous utilisons la fonction `find_and_replace` pour rechercher et remplacer le texte "le" par "la" dans une chaîne de caractères. La fonction prend en paramètres la chaîne de caractères à modifier, le texte à rechercher et le texte de remplacement. En utilisant une boucle while et la fonction `find` de la classe `string`, nous pouvons parcourir la chaîne de caractères et remplacer chaque occurrence du texte désiré.

Vous pouvez également utiliser des expressions régulières pour rechercher et remplacer du texte en utilisant la bibliothèque `<regex>`. Voici un exemple utilisant des expressions régulières pour remplacer tous les nombres dans une chaîne de caractères par le mot "Nombre" :

````C++
#include <iostream>
#include <string>
#include <regex>

int main() {
    std::string str = "Il y a 10 pommes et 5 bananes.";
    std::regex pattern("[0-9]+");
    std::string replace = "Nombre";

    str = std::regex_replace(str, pattern, replace);
    std::cout << str;

    return 0;
}
````
La sortie sera : "Il y a Nombre pommes et Nombre bananes."

## Plongée profonde
Maintenant que vous savez comment rechercher et remplacer du texte en utilisant des techniques différentes, il est important de noter certaines subtilités lors de l'utilisation de ces méthodes.

Tout d'abord, il est important de faire attention au contexte dans lequel vous effectuez votre recherche et remplacement. Si vous remplacez un texte dans une chaîne de caractères, assurez-vous que la longueur de votre texte de remplacement est la même que celle du texte à remplacer. Sinon, vous risquez de modifier la structure de la chaîne de caractères et d'introduire des erreurs dans votre code.

De plus, gardez à l'esprit que la fonction `find` renvoie la position de la première occurrence du texte recherché. Si vous souhaitez remplacer toutes les occurrences, vous devrez utiliser une boucle pour parcourir toutes les positions.

Enfin, si vous utilisez des expressions régulières, assurez-vous d'être précis dans votre modèle afin de ne pas remplacer du texte non désiré. Vous pouvez également utiliser des options spéciales pour spécifier si la recherche doit être sensible à la casse ou pas.

## Voir aussi
- [Documentation C++ sur les fonctions de recherche et de remplacement de la classe string](https://www.cplusplus.com/reference/string/string/find/)
- [Guide de référence regex en C++](https://docs.microsoft.com/fr-fr/cpp/standard-library/regex-class?view=vs-2019)
- [Exemples de recherches et remplacements avec regex en C++](https://www.geeksforgeeks.org/regex-in-c/)