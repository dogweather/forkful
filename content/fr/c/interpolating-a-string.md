---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'interpolation de chaînes est le processus d'insertion de variables dans une chaîne. Les programmeurs l'utilisent pour créer des chaînes dynamiques et facile à lire.

## Comment Faire:

Voici un exemple de code d'interpolation de chaîne en utilisant le format printf en C:

```C
#include <stdio.h>

int main() {
    char nom[] = "Pierre";
    printf("Bonjour, %s!\n", nom);
    
    return 0;
}
```

La sortie du code sera: `Bonjour, Pierre!`.

## Plongée Profonde

Historiquement, l'interpolation de chaîne n'était pas disponible dans les versions antérieures du langage C, obligeant les développeurs à utiliser des méthodes comme strcat() ou sprint(). Avec C99, nous avons le privilège d'utiliser la fonction snprintf() qui est beaucoup plus sûre.

Une alternative à l'interpolation de chaîne en C est la concaténation de chaînes, mais cela peut être plus verbeux et sujet à des erreurs.

En termes de mise en œuvre, le C utilise un tampon pour stocker les chaînes formatées avec printf(). Cette fonction écrit les valeurs des variables dans un tampon avant de le copier dans la chaîne de destination.

## Voir Aussi

Pour plus d'informations et de ressources sur l'interpolation de chaînes en C, vous pouvez consulter les liens suivants:
- [Interpolation de chaîne en C](https://www.cprogramming.com/tutorial/c/lesson4.html)
- [Documentation officielle sur les fonctions printf et sprintf](https://fr.cppreference.com/w/c/io/fprintf)
- [Introduction à l'interpolation de chaînes](http://www.learn-chinese-mandarin-language.com/C-string-interpolation-and-variable-replacement.html)