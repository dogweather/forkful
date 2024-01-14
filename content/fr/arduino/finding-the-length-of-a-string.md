---
title:    "Arduino: Trouver la longueur d'une chaîne de caractères"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Les strings, ou chaînes de caractères, sont une part essentielle de la programmation Arduino. Savoir comment trouver la longueur d'une string peut être très utile pour diverses applications telles que les mesures de capteurs ou l'affichage de données sur un écran LCD. Dans cet article, nous allons expliquer comment programmer cela sur votre Arduino.

## Comment faire

Pour trouver la longueur d'une string en utilisant l'Arduino, nous pouvons utiliser la fonction `strlen()`. Cette fonction prend une string en entrée et renvoie la longueur de cette string en tant que valeur entière en utilisant un compteur. Voici un exemple de code qui utilise cette fonction :

```Arduino
char nom[] = "Arduino";
int longueur = strlen(nom);
Serial.println("La longueur de la string est de : ");
Serial.println(longueur);
```

Dans cet exemple, nous avons déclaré une string appelée "nom" et initialisé sa valeur à "Arduino". Ensuite, nous avons utilisé la fonction `strlen()` pour trouver sa longueur et l'avons stockée dans une variable appelée "longueur". Enfin, nous avons affiché cette valeur sur le moniteur série à l'aide de la fonction `Serial.println()`.

Lorsque vous téléversez ce code sur votre Arduino et ouvrez le moniteur série, vous verrez que la longueur de la string "nom" est de 7, car il y a 7 caractères dans le mot "Arduino". Vous pouvez modifier la valeur de la string et expérimenter avec différentes valeurs pour voir comment la longueur change.

## Plongeon en profondeur

Il est important de noter que la fonction `strlen()` compte également les espaces et les symboles de ponctuation comme des caractères, ce qui peut avoir un impact sur la longueur totale de la string. De plus, cette fonction ne fonctionne qu'avec des char array, pas avec des variables de type String. Si vous avez besoin de trouver la longueur d'une variable String, il existe une fonction appelée `length()` qui fonctionne de la même manière que `strlen()`. Enfin, si vous souhaitez trouver la longueur d'une string à partir d'une position spécifique, vous pouvez utiliser la fonction `substring()` pour extraire une partie de la string et ensuite utiliser `strlen()` ou `length()` sur cette partie.

## Voir aussi

Pour en savoir plus sur la fonction `strlen()` et d'autres fonctions de manipulation de strings, vous pouvez consulter la documentation officielle d'Arduino : 
- https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/strlen/
- https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/
- https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/