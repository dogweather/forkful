---
title:                "Capitaliser une chaîne de caractères"
html_title:           "C: Capitaliser une chaîne de caractères"
simple_title:         "Capitaliser une chaîne de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous devez manipuler des chaînes de caractères dans un programme en C, vous pourriez avoir besoin de les capitaliser à un moment donné. La capitalisation d'une chaîne de caractère consiste à mettre la première lettre de chaque mot en majuscule. Dans cet article, nous allons vous montrer comment réaliser cette opération de manière efficace en utilisant le langage C.

## Comment faire

Pour capitaliser une chaîne de caractères en C, nous allons utiliser une fonction appelée `strcapitalize()` qui prend en paramètre la chaîne que nous voulons capitaliser. Voici un exemple de code qui illustre son utilisation :

```C
#include<stdio.h>
#include<string.h>

void strcapitalize(char *str) {
    // On met la première lettre en majuscule
    *str = toupper(*str);
    // On parcourt la chaîne pour trouver les espaces
    for(int i = 1; str[i] != '\0'; i++) {
        // Si on trouve un espace, on met la lettre suivante en majuscule
        if(str[i] == ' ' && str[i+1] != '\0') {
            str[i+1] = toupper(str[i+1]);
        }
    }
}

int main() {
    // On crée une chaîne de caractères à capitaliser
    char str[] = "bonjour tout le monde";
    // On appelle la fonction strcapitalize()
    strcapitalize(str);
    // On affiche la chaîne résultante
    printf("%s", str);
    return 0;
}
```

La sortie de ce code sera : `Bonjour Tout Le Monde`. Comme vous pouvez le voir, la fonction a bien capitalisé chaque mot de la chaîne originale.

## L'approfondissement

Maintenant que vous savez comment capitaliser une chaîne de caractères en utilisant la fonction `strcapitalize()` en C, vous pourriez vous demander comment elle fonctionne exactement. Dans les grandes lignes, cette fonction parcourt la chaîne caractère par caractère et utilise la fonction `toupper()` pour mettre la lettre en majuscule. Elle gère également les cas où il y a plusieurs espaces entre les mots ou lorsque la chaîne ne se termine pas par un espace.

Une autre astuce utile pour capitaliser une chaîne est d'utiliser la fonction `strtok()` pour séparer la chaîne en mots, puis d'utiliser la fonction `toupper()` pour mettre la première lettre de chaque mot en majuscule, avant de les réassembler avec la fonction `strcat()`.

## Voir aussi

Voici quelques liens utiles pour approfondir vos connaissances en programmation en C :

- [Documentation officielle de C](https://devdocs.io/c/)
- [Tutoriels de programmation en C](https://www.tutorialspoint.com/cprogramming/index.htm)
- [Communauté de développeurs en C](https://www.reddit.com/r/C_Programming/)