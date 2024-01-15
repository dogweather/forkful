---
title:                "Utilisation des expressions régulières"
html_title:           "C++: Utilisation des expressions régulières"
simple_title:         "Utilisation des expressions régulières"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

Pourquoi utiliser des expressions régulières en C++?

Les expressions régulières sont un outil puissant pour rechercher et manipuler des chaînes de caractères selon des modèles spécifiques. En utilisant des expressions régulières, vous pouvez augmenter l'efficacité de votre code et éviter des erreurs courantes liées à la manipulation de chaînes de caractères manuellement.

### Comment utiliser des expressions régulières en C++

Voici quelques exemples de code pour utiliser des expressions régulières en C++. Assurez-vous d'inclure la bibliothèque `<regex>` pour pouvoir utiliser les fonctions nécessaires.

```C++
#include <iostream>
#include <string>
#include <regex>

int main()
{
    // Définir une chaîne de caractères à analyser
    std::string texte = "Mon adresse email est test@test.com.";

    // Définir un modèle d'expression régulière pour rechercher une adresse email
    std::regex modele("(\\w+)(\\.\\w+)*@(\\w+)(\\.\\w+)*");

    // Utiliser la fonction `std::regex_search` pour rechercher une correspondance dans le texte
    if (std::regex_search(texte, modele))
    {
        std::cout << "Adresse email trouvée !" << std::endl;
    }

    return 0;
}
```

**Sortie:**
```
Adresse email trouvée !
```

Vous pouvez également utiliser des expressions régulières pour remplacer du texte dans une chaîne de caractères. Dans l'exemple suivant, nous remplacerons toutes les occurrences de "bienvenue" par "bonjour" dans la chaîne de caractères.

```C++
#include <iostream>
#include <string>
#include <regex>

int main()
{
    std::string texte = "Bienvenue à tous les nouveaux utilisateurs !";

    // Définir un modèle d'expression régulière pour rechercher et remplacer du texte
    std::regex modele("bienvenue");

    // Utiliser la fonction `std::regex_replace` pour remplacer le texte
    std::cout << std::regex_replace(texte, modele, "bonjour") << std::endl;

    return 0;
}
```

**Sortie:**
```
Bonjour à tous les nouveaux utilisateurs !
```

### Plongez plus profondément dans les expressions régulières

Il existe de nombreux modèles et fonctions disponibles pour utiliser des expressions régulières en C++. Vous pouvez consulter [la documentation de la bibliothèque `<regex>`](https://en.cppreference.com/w/cpp/regex) pour découvrir toutes les possibilités.

Pour les débutants, il peut être utile de consulter des tutoriels en ligne pour comprendre comment créer des modèles et utiliser efficacement les fonctions de la bibliothèque. Vous pouvez également pratiquer en utilisant un outil en ligne comme [Regex101](https://regex101.com/) pour tester vos modèles et voir des exemples d'utilisation.

---
Voir aussi:
- [C++ Regex Tutorial](https://www.geeksforgeeks.org/regular-expressions-in-c-implementation/)
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Online regex tester and debugger](https://regex101.com/)