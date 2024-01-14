---
title:    "C++: Utilisation des expressions régulières"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi

Les expressions régulières sont un outil puissant pour manipuler du texte dans un programme C++. Elles permettent une recherche et un remplacement flexibles de motifs de caractères, rendant ainsi la manipulation des données plus efficace et moins fastidieuse.

## Comment faire

Pour utiliser les expressions régulières en programmation C++, il est nécessaire d'inclure la bibliothèque <regex> dans le code. Ensuite, il suffit d'utiliser la classe std::regex pour créer un objet d'expression régulière et de l'utiliser dans des fonctions telles que regex_search et regex_replace. Voici un exemple de code montrant comment rechercher et remplacer un pattern dans une chaîne de caractères :

```C++
#include <iostream>
#include <regex>
using namespace std;

int main() {
    // Création d'une expression régulière pour trouver des adresses emails
    regex reg("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}");

    // Chaîne de caractères de test
    string texte = "Mon adresse email est test@mail.com. N'hésitez pas à me contacter !";

    // Recherche dans le texte
    smatch correspondances;
    regex_search(texte, correspondances, reg);

    // Affichage des correspondances trouvées
    cout << "L'adresse email trouvée est : " << correspondances[0] << endl;

    // Remplacement de l'adresse email par "********"
    string texteModifie = regex_replace(texte, reg, "*********");
    cout << "Texte modifié : " << texteModifie << endl;

    return 0;
}
```

L'output de ce code sera :

```
L'adresse email trouvée est : test@mail.com
Texte modifié : Mon adresse email est *********. N'hésitez pas à me contacter !
```

## Plongée en profondeur

Les expressions régulières en C++ sont basées sur la syntaxe ECMAScript, c'est pourquoi elles partagent de nombreuses similitudes avec les expressions régulières utilisées dans d'autres langages de programmation tels que JavaScript ou Python. Cependant, il y a quelques différences à noter, notamment en ce qui concerne les caractères d'échappement et les flags de recherche.

Il est également important de comprendre comment fonctionnent les expressions régulières en termes de temps et d'espace. Selon la complexité du motif et la taille du texte, l'utilisation d'expressions régulières peut entraîner une baisse des performances. Il est donc essentiel d'optimiser votre code pour éviter les problèmes de temps d'exécution ou de consommation de mémoire.

Enfin, il existe de nombreux outils en ligne pour tester vos expressions régulières en temps réel et les optimiser avant de les implémenter dans votre code. Prenez le temps d'explorer ces ressources pour de meilleurs résultats.

## Voir aussi

- [Documentation officielle sur les expressions régulières en C++](https://en.cppreference.com/w/cpp/regex)
- [Outil de test et de validation des expressions régulières en ligne](https://regex101.com/)
- [Guide de référence pour la syntaxe des expressions régulières ECMAScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)