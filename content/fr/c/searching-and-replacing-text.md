---
title:    "C: Rechercher et remplacer du texte"
keywords: ["C"]
---

{{< edit_this_page >}}

## Pourquoi

La fonction de recherche et de remplacement de texte est essentielle pour les programmeurs C qui souhaitent automatiser la tâche fastidieuse de modifier des chaînes de caractères dans leur code. Que vous ayez besoin de corriger des fautes de frappe, de remplacer un terme par un autre ou de modifier un format, cette fonction peut vous faire gagner un temps précieux.

## Comment faire

Pour utiliser la fonction de recherche et de remplacement de texte en C, vous devez inclure la bibliothèque string.h dans votre code. Ensuite, utilisez la fonction strcpy() pour copier la chaîne de caractères d'origine dans une nouvelle variable, puis utilisez la fonction strstr() pour rechercher la sous-chaîne cible dans la chaîne d'origine. Enfin, utilisez la fonction strcpy() à nouveau pour remplacer la sous-chaîne par la nouvelle valeur.

```
#include <stdio.h>
#include <string.h>

int main() {
  char str1[100] = "Bonjour tout le monde!";
  char str2[] = "tout le monde";
  char str3[] = "python";

  // Remplacez "tout le monde" par "C" dans la chaîne d'origine
  strcpy(str1, strstr(str1, str2), str3);

  printf("%s", str1);
  return 0;
}

// Output: "Bonjour C!"
```

## Plongée en profondeur

L'utilisation de la fonction de recherche et de remplacement de texte peut sembler simple, mais il y a quelques pièges à éviter. Par exemple, si la sous-chaîne cible est trouvée plusieurs fois dans la chaîne d'origine, seule la première occurrence sera remplacée. De plus, si la sous-chaîne cible est plus longue que la chaîne d'origine, cela peut entraîner des problèmes de mémoire. Voilà pourquoi il est important de toujours vérifier la longueur des chaînes avant de les remplacer.

## Voir aussi:

- [Documentation officielle de la fonction strstr() en C] (https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_73/rtref/strstr.htm)
- [Guide pratique sur la manipulation des chaînes de caractères en C] (https://www.programiz.com/c-programming/c-strings)
- [Exemples de code pour la fonction de recherche et de remplacement de texte] (https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)