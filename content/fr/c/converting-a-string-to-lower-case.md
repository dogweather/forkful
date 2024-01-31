---
title:                "Conversion d'une chaîne de caractères en minuscules"
date:                  2024-01-20T17:37:47.591414-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversion d'une chaîne de caractères en minuscules"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Convertir une chaîne en minuscules, c'est transformer tous les caractères alphabétiques en leur équivalent minuscule. Les programmeurs font ça pour uniformiser les données, faciliter les comparaisons de textes ou préparer une chaîne à des opérations comme le stockage ou la recherche.

## Comment faire :
Voici un code simple pour convertir une chaîne en minuscules avec la librairie standard de C :

```c
#include <stdio.h>
#include <ctype.h>

void convertToLowercase(char *str) {
    while (*str) {
        *str = tolower((unsigned char) *str);
        str++;
    }
}

int main() {
    char myString[] = "Bonjour, le MONDE!";
    convertToLowercase(myString);
    printf("%s\n", myString); // Output: bonjour, le monde!
    return 0;
}
```

## Plongée profonde
Historiquement, la conversion de casse en C est résolue par les fonctions `tolower()` et `toupper()` définies dans `ctype.h`. Ces fonctions sont simples mais robustes, et elles tiennent compte des paramètres régionaux (locales). Elles traitent les caractères ASCII mais ne sont pas adéquates pour l'Unicode, où la conversion peut être beaucoup plus complexe.

Des alternatives existent, comme la GNU C Library (glibc) qui propose des fonctions pouvant gérer l'Unicode. Vous pouvez aussi écrire votre propre fonction de conversion qui traiterait des cas spécifiques de caractères accentués en fonction de votre besoin.

Côté implémentation, il est crucial de caster le caractère en `unsigned char` avant de le passer à `tolower()`, pour éviter un comportement indéfini si `char` est signé et contient un caractère hors de l'ASCII standard.

## Voir également
- La documentation de `tolower` sur le site du [CPP Reference](https://en.cppreference.com/w/c/string/byte/tolower)
- Une discussion sur la conversion de casse pour l'Unicode sur [Stack Overflow](https://stackoverflow.com/questions/23396543/how-to-tolower-in-c-that-supports-unicode)
- Informations sur la locale et les fonctions associées en C dans le [GNU C Library manual](https://www.gnu.org/software/libc/manual/html_node/Locales.html)
