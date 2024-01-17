---
title:                "Transformer une chaîne en minuscules."
html_title:           "C: Transformer une chaîne en minuscules."
simple_title:         "Transformer une chaîne en minuscules."
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi?

La conversion d'une chaîne de caractères en minuscules est une opération couramment utilisée par les programmeurs en C. Elle consiste à modifier toutes les lettres majuscules d'une chaîne en lettres minuscules. Cette opération peut être utile pour faciliter la comparaison de chaînes de caractères ou pour l'affichage de texte en minuscules.

## Comment faire:

Voici un exemple de code en C pour convertir une chaîne de caractères en minuscules :

```
#include <stdio.h>
#include <string.h>

int main() {
  char str[50];
  printf("Entrez une chaîne de caractères: ");
  gets(str);

  // Boucle pour parcourir chaque caractère de la chaîne
  for (int i = 0; str[i] != '\0'; i++) {
    // Si le caractère est une lettre majuscule, on le convertit en minuscule 
    if (str[i] >= 'A' && str[i] <= 'Z') {
      str[i] = str[i] + 32;
    }
  }

  // Affichage de la chaîne en minuscules
  printf("La chaîne convertie en minuscules est: %s", str);

  return 0;
}
```

Exemple d'entrée et de sortie:

```
Entrez une chaîne de caractères: Hello World
La chaîne convertie en minuscules est: hello world
```

## Plongée en profondeur:

Historiquement, les lettres majuscules et minuscules étaient codées différemment dans les ordinateurs. La conversion en minuscules était donc nécessaire pour les manipuler correctement. De nos jours, différentes fonctions et bibliothèques offrent des alternatives à la conversion en minuscules, comme les fonctions `tolower` ou `strlwr`.

La conversion en minuscules peut également être implémentée en utilisant des opérations de bits plutôt qu'une boucle. Cette méthode est généralement plus efficace en termes de performance, mais elle est plus complexe à mettre en œuvre.

## Voir aussi:

Pour plus d'informations sur la conversion de chaînes en minuscules en C, consultez les sources suivantes:

- [Documentation officielle sur la fonction `tolower` en C](https://www.tutorialspoint.com/c_standard_library/c_function_tolower.htm)
- [Documentation officielle sur la fonction `strlwr` en C](https://www.tutorialspoint.com/c_standard_library/c_function_strlwr.htm)
- [Article sur la conversion en minuscules en utilisant des opérations de bits en C](https://embeddedbits.org/converting-uppercase-to-lowercase-and-vice-versa-in-c-using-bit-hacks/)