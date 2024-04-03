---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:30.086073-07:00
description: "Comment : C n'a pas de fonction int\xE9gr\xE9e pour la conversion de\
  \ cha\xEEne en minuscules directement, contrairement \xE0 certains langages de haut\
  \ niveau.\u2026"
lastmod: '2024-03-13T22:44:58.356865-06:00'
model: gpt-4-0125-preview
summary: "C n'a pas de fonction int\xE9gr\xE9e pour la conversion de cha\xEEne en\
  \ minuscules directement, contrairement \xE0 certains langages de haut niveau."
title: "Convertir une cha\xEEne en minuscules"
weight: 4
---

## Comment :
C n'a pas de fonction intégrée pour la conversion de chaîne en minuscules directement, contrairement à certains langages de haut niveau. Cependant, le processus peut être facilement mis en œuvre en utilisant les fonctions de la bibliothèque standard C. Ci-dessous, un guide étape par étape et un exemple illustrant comment convertir une chaîne en minuscules.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("Original : %s\n", text);

    toLowerCase(text);
    printf("Minuscules : %s\n", text);

    return 0;
}
```

**Exemple de sortie :**

```
Original : Hello, World!
Minuscules : hello, world!
```

Dans cet exemple, la fonction `toLowerCase` itère à travers chaque caractère de la chaîne d'entrée, le convertissant en son équivalent en minuscules à l'aide de la fonction `tolower` de `ctype.h`. La modification est faite sur place, modifiant la chaîne originale.

## Approfondissement
La fonction `tolower` utilisée dans l'exemple ci-dessus fait partie de la bibliothèque standard C, spécifiquement dans le fichier d'en-tête `ctype.h`. Elle fonctionne en fonction de la locale courante, mais pour la locale standard "C", elle gère l'ensemble de caractères ASCII où de 'A' à 'Z' sont convertis en 'a' à 'z'.

Historiquement, le traitement de l'encodage des caractères et de la conversion des cas en C était étroitement lié à l'ensemble de caractères ASCII, limitant son utilité dans les applications internationales ou localisées où les caractères hors ASCII sont courants. Les langages de programmation modernes pourraient offrir des méthodes de chaîne intégrées pour effectuer la conversion de cas en tenant compte de la locale et des caractères Unicode, ce que C manque nativement.

Dans des scénarios nécessitant une manipulation extensive de texte, en particulier avec des caractères non ASCII, les programmeurs pourraient envisager d'utiliser des bibliothèques offrant une meilleure prise en charge de l'internationalisation, telles que ICU (International Components for Unicode). Cependant, pour la plupart des applications traitant du texte ASCII, l'approche démontrée est efficace et simple. Elle met en évidence la propension de C à donner aux programmeurs le contrôle sur la manipulation des données, bien qu'un peu plus de travail soit impliqué par rapport aux langages de niveau supérieur.
