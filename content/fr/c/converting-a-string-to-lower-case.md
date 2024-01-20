---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

La conversion d'une chaîne de caractères en minuscules consiste à changer toutes les lettres majuscules en minuscules. Les programmeurs font cela pour normaliser les données textuelles, facilitant ainsi les comparaisons string.

## Comment faire :

Voici une méthode simple pour convertir une chaîne en minuscules en utilisant la fonction `tolower()` de la bibliothèque `<ctype.h>` en C.

```C
#include <ctype.h>
#include <stdio.h>

void string_to_lower(char* s)
{
    for(int i = 0; s[i]; i++){
    s[i] = tolower(s[i]);
    }
}

int main()
{
    char string[] = "Bonjour, Programmation C!";
    string_to_lower(string);
    printf("%s\n", string); // affiche: "bonjour, programmation c!"
    return 0;
}
```

## Plongée en profondeur :

Historiquement, la conversion de chaînes en minuscules a toujours été un outil important pour les manipulateurs de texte. En C, la fonction `tolower()` est une fonction standard fournie par la bibliothèque `<ctype.h>`. Elle convertit un caractère en sa forme minuscule et la retourne.

Sinon, vous pourriez faire la conversion vous-même en ajoutant une valeur fixe (la différence dans le tableau ASCII entre les lettres majuscules et minuscules) aux lettres majuscules. Cependant, cela suppose que vous travaillez avec du texte en ASCII et peut causer des problèmes avec d'autres encodages.

La fonction `tolower()` utilise la configuration locale actuelle pour déterminer ce que ça signifie d'être une majuscule et une minuscule, ce qui rend sa mise en œuvre plus générale.

## Voir aussi :

Pour plus d'informations sur `tolower()` et la bibliothèque `<ctype.h>`, consultez ces liens :
- Documentation officielle de C : https://docs.microsoft.com/fr-fr/cpp/c-language/c-language-reference?view=msvc-160
- Wikipédia: https://fr.wikipedia.org/wiki/Bibliothèque_standard_du_C