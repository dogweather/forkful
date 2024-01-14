---
title:                "C++: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi capitaliser une chaîne de caractères en C++?

Capitaliser une chaîne de caractères en C++ peut sembler une tâche simple et banale, mais cela peut s'avérer très utile dans certains cas. Par exemple, si vous souhaitez afficher un texte en majuscules dans votre programme, vous aurez besoin de connaître les différentes méthodes pour capitaliser une chaîne de caractères. Dans cet article, nous allons explorer pourquoi et comment capitaliser une chaîne de caractères en C++, ainsi que plonger plus en profondeur dans cette fonctionnalité.

## Comment capitaliser une chaîne de caractères en C++?

Il existe différentes façons de capitaliser une chaîne de caractères en C++. Nous allons vous montrer deux méthodes couramment utilisées pour réaliser cette tâche.

```C++
#include <iostream>
#include <string>

using namespace std;

int main()
{
    // Création d'une chaîne de caractères en minuscules
    string texte = "bonjour tout le monde";
    
    // Méthode 1 : Utilisation de la fonction transform()
    transform(texte.begin(), texte.end(), texte.begin(), ::toupper);
    cout << texte << endl;
    
    //Méthode 2 : Utilisation de la boucle for et de la fonction toupper()
    for(int i=0; i<texte.length(); i++)
    {
        texte[i] = toupper(texte[i]);
    }
    cout << texte << endl;
    
    return 0;
}
```

**Output:**

BONJOUR TOUT LE MONDE
BONJOUR TOUT LE MONDE

La première méthode utilise la fonction `transform()` qui prend en paramètres un itérateur de début, un itérateur de fin et une opération à appliquer sur chaque caractère. Dans notre cas, nous utilisons `::toupper` pour convertir chaque caractère en majuscule.

La deuxième méthode utilise une boucle for pour parcourir chaque caractère de la chaîne de caractères et la fonction `toupper()` pour la convertir en majuscule.

Vous pouvez choisir la méthode qui vous convient le mieux en fonction de vos préférences et de la complexité de votre programme.

## Plongée plus profonde

Maintenant, intéressons-nous à la mécanique derrière la fonction `transform()` et la fonction `toupper()` utilisées dans les exemples ci-dessus. Lorsque nous utilisons `::toupper`, nous faisons appel à une fonction membre statique de la classe `locale`. Cette fonction prend en paramètre un caractère et renvoie sa version en majuscule, selon la locale actuellement utilisée. Si aucune locale n'est spécifiée, la locale "C" sera utilisée par défaut.

La fonction `transform()` quant à elle, utilise deux itérateurs pour parcourir la chaîne de caractères et appliquer l'opération `::toupper` sur chaque caractère. Il est important de noter que cette fonction n'est disponible que depuis le C++11.

## Voir également
- [Documentation sur `transform()`](https://en.cppreference.com/w/cpp/algorithm/transform)
- [Documentation sur la fonction `toupper()`](https://en.cppreference.com/w/cpp/string/byte/toupper)
- [Article sur les locales en C++](https://www.geeksforgeeks.org/locale-class-in-c/)

Merci d'avoir lu cet article sur la capitalisation de chaînes de caractères en C++. J'espère que cela vous sera utile dans vos futurs projets. N'hésitez pas à partager vos astuces pour capitaliser une chaîne de caractères dans les commentaires ci-dessous. À bientôt pour de nouveaux articles sur la programmation en C++!