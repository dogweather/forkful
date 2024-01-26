---
title:                "Organisation du code en fonctions"
date:                  2024-01-26T01:09:25.409024-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisation du code en fonctions"
programming_language: "C"
category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Organiser le code en fonctions consiste à diviser le code en blocs réutilisables qui accomplissent des tâches spécifiques. Cela rend le code plus facile à lire, à déboguer et à maintenir.

## Comment faire :
Prenons un exemple simple : disons que vous voulez additionner deux nombres plusieurs fois.

Sans fonctions :
```C
#include <stdio.h>

int main() {
    int sum1 = 5 + 3;
    printf("Somme1 : %d\n", sum1);
    
    int sum2 = 2 + 8;
    printf("Somme2 : %d\n", sum2);
    
    // Plus d'additions ici...
    
    return 0;
}
```

Avec fonctions :
```C
#include <stdio.h>

int add(int a, int b) {
    return a + b;
}

int main() {
    int sum1 = add(5, 3);
    printf("Somme1 : %d\n", sum1);
    
    int sum2 = add(2, 8);
    printf("Somme2 : %d\n", sum2);
    
    // Utiliser la fonction add() pour plus d'additions...
    
    return 0;
}
```

Sortie :
```
Somme1 : 8
Somme2 : 10
```

## Approfondissement
Avant que le C n'ait des fonctions, la programmation était souvent linéaire, un peu comme une recette de cuisine. Mais à mesure que les programmes se sont agrandis, la duplication de code est devenue un problème. Les fonctions ont été la solution - elles nous ont permis d'exécuter le même bloc de code depuis différentes parties d'un programme sans avoir à le réécrire à chaque fois. Cela permet non seulement de gagner de l'espace mais aussi du temps lors des mises à jour : changez la fonction à un endroit, et chaque partie de votre code qui l'utilise est mise à jour.

Les alternatives aux fonctions pourraient inclure le code en ligne, les macros ou la programmation par copier-coller, mais celles-ci peuvent conduire à un code gonflé, sujet aux erreurs et difficile à maintenir. Les fonctions, en revanche, encapsulent la fonctionnalité, définissent des interfaces claires et peuvent réduire les effets de bord grâce à une utilisation adéquate de la portée.

Lorsque vous implémentez des fonctions, considérez quelques détails : d'abord, essayez de faire en sorte qu'elles ne fassent qu'une seule chose – c'est ce qu'on appelle le Principe de Responsabilité Unique. Deuxièmement, les noms sont importants – choisissez des noms descriptifs pour les fonctions et leurs paramètres afin de rendre votre code auto-documenté.

## Voir également
Pour plus d'informations sur les fonctions en C, jetez un œil à ces ressources :

- Référence de la bibliothèque standard C : https://en.cppreference.com/w/c/header
- C Programming: A Modern Approach de K.N. King : Un livre avec un approfondissement sur les fonctions.
- Learn-C.org : section Fonctions : https://www.learn-c.org/en/Functions