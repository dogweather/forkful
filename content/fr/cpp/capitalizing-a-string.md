---
title:                "C++: Capitalisation d'une chaîne de caractères"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

On se retrouve souvent dans des situations où l'on a besoin d'afficher un mot ou une phrase avec la première lettre en majuscule. Peut-être que l'on souhaite créer un programme qui permet de saisir un nom et de le renvoyer avec la première lettre en majuscule, ou peut-être que l'on veut simplement améliorer la lisibilité des messages dans notre application. Dans tous les cas, la capitalisation d'une chaîne de caractères est une tâche courante en programmation et il est important de savoir comment le faire correctement. 

## Comment faire

La bonne nouvelle, c'est qu'en C++, il existe une fonction spécialement conçue pour la capitalisation d'une chaîne de caractères : `toupper()` de la bibliothèque `<cctype>`. Cette fonction prend en paramètre un caractère et renvoie sa version en majuscule. Nous pouvons donc l'utiliser pour parcourir chaque caractère de notre chaîne de caractères et le modifier si nécessaire. Voici un exemple de code en utilisant cette fonction :

```c++
#include <iostream>
#include <cctype>
#include <string>

int main() {
    // Demander à l'utilisateur d'entrer une chaîne de caractères
    std::cout << "Entrez une phrase à capitaliser : ";
    std::string phrase;
    std::getline(std::cin, phrase);

    // Parcourir chaque caractère de la chaîne
    for (int i = 0; i < phrase.length(); i++) {
        // Vérifier si le caractère est une lettre minuscule
        if (islower(phrase[i])) {
            // Utiliser la fonction toupper() pour changer le caractère en majuscule
            phrase[i] = toupper(phrase[i]);
        } 
    }

    // Afficher la chaîne de caractères capitalisée
    std::cout << "Voici la phrase capitalisée : " << phrase << std::endl;

    return 0;
}
```

Voici un exemple d'entrée et de sortie de ce programme :

```
Entrez une phrase à capitaliser : bonjour à tous
Voici la phrase capitalisée : BONJOUR À TOUS
```

Cependant, il est important de noter que cette méthode ne fonctionne que si notre chaîne ne comporte que des caractères de l'alphabet. Si elle contient des chiffres ou des caractères spéciaux, ils seront également modifiés en majuscules. 

## Plongée profonde

Si nous souhaitons capitaliser des chaînes de caractères de manière plus précise, il peut être nécessaire d'utiliser une fonction un peu plus complexe. Heureusement, la bibliothèque standard de C++ offre une autre fonction pour cela : `std::toupper` de la bibliothèque `<locale>`. Cette fonction prend en paramètre un caractère et un objet `std::locale` et renvoie la version en majuscule du caractère dans la langue spécifiée par la locale. Par exemple, si nous voulons capitaliser une chaîne en français, nous pouvons utiliser la locale `fr_FR` comme ceci :

```c++
// Inclure également <locale>
#include <locale>

// ...

// Parcourir chaque caractère de la chaîne
for (int i = 0; i < phrase.length(); i++) {
    // Vérifier si le caractère est une lettre minuscule en utilisant la locale française
    if (std::tolower(phrase[i], std::locale("fr_FR")) == phrase[i]) {
        // Utiliser la fonction std::toupper en spécifiant à nouveau la locale française
        phrase[i] = std::toupper(phrase[i], std::locale("fr_FR"));
    }
}
```

Bien sûr, le choix de la locale dépendra de la langue et des règles de capitalisation que vous souhaitez appliquer à votre chaîne.

## Voir aussi

- [Documentation sur la fonction `toupper()`](https://www.cplusplus.com/reference/cctype/toupper/)
- [Documentation sur la fonction `std::toupper()`](https://www.cplusplus.com/reference/locale/toupper/)
- [Tutoriel sur la gestion des strings en C++](https://www.tutoriels.eu/cpp/strings/)