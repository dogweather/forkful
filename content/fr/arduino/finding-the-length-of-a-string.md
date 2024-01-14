---
title:                "Arduino: Trouver la longueur d'une chaîne de caractères"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles on pourrait vouloir trouver la longueur d'une chaîne de caractères en Arduino. Cela peut être utile pour traiter des entrées de l'utilisateur, pour effectuer des comparaisons de chaînes ou pour compter le nombre de caractères dans une chaîne.

## Comment faire

Pour trouver la longueur d'une chaîne en Arduino, nous pouvons utiliser la fonction `length()`. Cette fonction prend en paramètre une chaîne de caractères et renvoie le nombre de caractères dans celle-ci. Voici un exemple de code Arduino et sa sortie :

```Arduino
String myString = "Bonjour";
int length = myString.length();
Serial.println(length);

// Output: 7
```

Nous pouvons également utiliser la fonction `strlen()` qui fonctionne de la même manière. Toutefois, cette fonction n'est pas disponible pour les chaînes de caractères définies avec la classe `String`. Nous devons donc utiliser des tableaux de caractères (`char`) et la fonction `strcpy()` pour copier la chaîne dans ce tableau, comme le montre l'exemple suivant :

```Arduino
char myString[] = "Bonjour";
int length = strlen(myString);
Serial.println(length);

// Output : 7
```

Il est important de noter que cette fonction renverra le nombre de caractères dans la chaîne, y compris l'espace, les chiffres et les caractères spéciaux.

## Plongée en profondeur

La fonction `length()` utilise en réalité la méthode `size()` qui est définie dans la classe `String`. Cette méthode parcourt la chaîne de caractères et utilise la fonction `strlen()` pour calculer la taille. Si vous utilisez la fonction `length()` sur une chaîne vide, elle renverra toujours 1 car elle compte le caractère de fin de chaîne.

## Voir aussi

- [Documentation officielle sur la fonction `length()`](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/length/)
- [Documentation officielle sur la fonction `strlen()`](https://www.arduino.cc/reference/en/language/functions/strings/strlen/)
- [Documentation officielle sur la classe `String`](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutorial YouTube sur la manipulation de chaînes en Arduino](https://www.youtube.com/watch?v=I5IplsZ_vmc)