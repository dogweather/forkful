---
title:    "C++: Mettre en majuscule une chaîne de caractères"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur C++, vous avez probablement rencontré des situations où vous avez besoin de capitaliser une chaîne de caractères. Cela peut sembler trivial, mais c'est en fait une tâche importante dans de nombreuses applications. Dans cet article, nous allons explorer pourquoi capitaliser une chaîne de caractères est important et comment le faire efficacement en C++.

## Comment faire

Pour capitaliser une chaîne de caractères en C++, nous pouvons utiliser la fonction `toupper` de la bibliothèque `<cctype>`. Cette fonction prend en paramètre un caractère et renvoie sa version en majuscule. Nous pouvons donc l'utiliser pour parcourir la chaîne de caractères et convertir chaque caractère en majuscule.

Voici un exemple de code en C++ montrant comment capitaliser une chaîne de caractères :

```C++
#include <iostream>
#include <cctype>

int main() {
    // initialiser une chaîne de caractères
    std::string chaine = "bonjour le monde";

    // parcourir chaque caractère et le convertir en majuscule
    for (int i = 0; i < chaine.length(); i++) {
        chaine[i] = toupper(chaine[i]);
    }

    // afficher la chaîne de caractères capitalisée
    std::cout << chaine << std::endl;

    return 0;
}

// output: BONJOUR LE MONDE

```

## Plongée profonde

Il est important de noter que la fonction `toupper` ne modifiera pas les caractères spéciaux ou les chiffres. Elle ne convertira que les lettres en majuscules. De plus, cette méthode ne fonctionnera que pour les chaînes de caractères dans la norme ASCII. Si vous travaillez avec des chaînes de caractères dans d'autres normes, vous devrez utiliser d'autres méthodes pour les capitaliser.

Une autre chose à prendre en compte est que la fonction `toupper` est sensible à la localisation. Cela signifie qu'elle peut produire des résultats différents en fonction de la langue et du pays dans lequel votre programme est exécuté. Si vous voulez vous assurer que votre programme fonctionne correctement dans toutes les situations, vous devrez peut-être utiliser des algorithmes plus complexes pour capitaliser une chaîne de caractères.

## Voir aussi

- [Documentation de la Bibliothèque Standard de C++](https://en.cppreference.com/w/cpp/header/cctype)
- [Article sur la localisation en C++](https://en.wikipedia.org/wiki/C%2B%2B_localisation)