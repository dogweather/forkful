---
title:    "C++: Trouver la longueur d'une chaîne de caractères"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Pourquoi
Le calcul de la longueur d'une chaîne de caractères est une tâche courante en programmation qui peut être utile dans de nombreuses situations. Cela vous permet de connaître le nombre exact de caractères dans une chaîne, ce qui vous permettra de manipuler et de traiter les données plus efficacement. Cela peut également être utile pour vérifier si une chaîne est vide ou si elle dépasse une longueur maximale définie.

## Comment faire
Le calcul de la longueur d'une chaîne est assez simple en C++. Il suffit d'utiliser la fonction intégrée `length()` ou `size()` en utilisant la notation pointée (`->`) pour accéder à la longueur de la chaîne. Voici un exemple de code :
```C++
#include <iostream> 

int main() { 
    std::string chaine = "Bonjour le monde !"; 
    int longueur = chaine.length(); 
    std::cout << "La longueur de la chaîne est de " << longueur << " caractères." << std::endl; 

    return 0; 
}
```
Output : La longueur de la chaîne est de 18 caractères.

On peut également utiliser la boucle `for` pour parcourir une chaîne de caractères et compter le nombre de caractères un à un. Voici un exemple de code :
```C++
#include <iostream> 

int main() { 
    std::string chaine = "Bonjour le monde !"; 
    int longueur = 0; 

    for(int i = 0; chaine[i] != '\0'; ++i) {
        longueur++; 
    }

    std::cout << "La longueur de la chaîne est de " << longueur << " caractères." << std::endl; 

    return 0; 
}
```
Output : La longueur de la chaîne est de 18 caractères.

## Plongée en profondeur
En C++, chaque chaîne de caractères est terminée par un caractère nul (`\0`), qui est automatiquement ajouté à la fin lors de l'initialisation ou de la concaténation de chaînes de caractères. Cela signifie que pour compter exactement le nombre de caractères dans une chaîne, il suffit de parcourir la chaîne et de s'arrêter lorsque le caractère nul est rencontré.

Il est également important de noter que la fonction `length()` renvoie un `size_t`, qui peut être différent d'un entier régulier (`int`). Il est donc préférable d'utiliser `size_t` pour déclarer la variable de longueur de la chaîne.

## Voir aussi
- [Documentation sur la fonction `length()` en C++](https://www.cplusplus.com/reference/string/string/length/)
- [Documentation sur la fonction `size()` en C++](https://www.cplusplus.com/reference/string/string/size/)