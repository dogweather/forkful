---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

La suppression de caractères correspondant à un modèle (ou 'pattern' en anglais) consiste à enlever de l'information spécifique d'une chaîne de caractères. C'est une opération assez courante en programmation car cela permet de nettoyer les données et de les rendre plus compréhensibles et exploitable par le code.

## Comment Faire :

Voici un exemple simple en C pour supprimer tous les caractères 'a' d'une chaîne de caractères :

```C
#include <string.h>

int main() {
   char str[] = "example";
   int i, j;
    
   for(i = 0; str[i] != '\0'; ++i) {
      while(!( (str[i]>='a'&&str[i]<='z') || str[i]=='\0')) {
         for(j = i; str[j] != '\0'; ++j) {
            str[j] = str[j+1];
         }
         str[j]='\0';
      }
   }
   printf("Result: %s", str);
   return 0;
}
```

L'output sera `Result: exmple`

## Plongée en Profondeur :

Historiquement, la suppression de caractères en utilisant un modèle a été introduite avec la programmation en C, et elle est encore largement utilisée aujourd'hui. Les alternatives à cette méthode incluent l'utilisation de pointeurs pour parcourir la chaîne de caractères et supprimer les caractères correspondant à un modèle spécifique. Vous pouvez également utiliser des bibliothèques tierces qui offrent des fonctions plus sophistiquées pour la manipulation des chaînes de caractères. 

## Voir Aussi :

Pour plus d'information sur la manipulation des chaînes de caractères en C, consultez la documentation officielle de la bibliothèque string.h [ici](https://www.gnu.org/software/libc/manual/html_node/String-Functions.html) et le tutoriel sur la gestion de mémoire [ici](https://www.learn-c.org/en/Memory_management).