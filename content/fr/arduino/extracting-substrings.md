---
title:    "Arduino: Extraction de sous-chaînes"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Pourquoi

Si vous travaillez avec des chaînes de caractères dans vos projets Arduino, vous pouvez avoir besoin d'extraire une sous-chaîne pour effectuer certaines opérations ou pour obtenir une partie précise d'une chaîne plus longue. Dans cet article, nous allons vous montrer comment faire cela en utilisant le langage de programmation Arduino.

# Comment Faire

Pour extraire une sous-chaîne dans Arduino, vous aurez besoin de deux fonctions : `substring()` et `indexOf()`. La fonction `substring()` prend deux paramètres : la position de départ de la sous-chaîne et la longueur de la sous-chaîne à extraire. La fonction `indexOf()` est utilisée pour trouver la position d'un caractère spécifique dans une chaîne.

Voici un exemple de code qui montre comment utiliser ces deux fonctions pour extraire une sous-chaîne à partir d'une chaîne plus longue :

```Arduino
// Déclaration de la chaîne de caractères
String chaine = "Bonjour tout le monde !";

// Trouver la position du caractère "-"
int position = chaine.indexOf("!");

// Extraire la sous-chaîne
String sousChaine = chaine.substring(0, position);
```

La variable `sousChaine` contiendra maintenant la valeur "Bonjour tout le monde". Vous pouvez également utiliser cette technique pour extraire une partie d'un nombre ou d'une chaîne de caractères plus complexe.

# Plongeons Plus Profondément

En utilisant ces deux fonctions, vous pouvez également extraire une sous-chaîne à partir d'une position spécifique jusqu'à la fin de la chaîne. Pour cela, il suffit de ne spécifier qu'un seul paramètre dans la fonction `substring()`, qui sera la position de départ de la sous-chaîne.

De plus, vous pouvez également utiliser la fonction `lastIndexOf()` pour trouver la position du dernier occurrence d'un caractère dans une chaîne. Cela peut être utile pour extraire une sous-chaîne à partir de la fin de la chaîne plutôt que du début.

# Voir aussi

Pour plus d'informations sur la manipulation des chaînes de caractères en Arduino, vous pouvez consulter les liens suivants :

- [Documentation officielle d'Arduino sur les chaînes de caractères](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutoriel vidéo sur l'extraction des sous-chaînes en Arduino](https://www.youtube.com/watch?v=gzLgjQ8vqQg)
- [Forum de la communauté Arduino](https://forum.arduino.cc/)

Maintenant que vous savez comment extraire des sous-chaînes, vous pouvez les utiliser dans vos projets Arduino pour manipuler et traiter des données textuelles plus efficacement. Amusez-vous bien !