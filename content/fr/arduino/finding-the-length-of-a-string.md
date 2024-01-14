---
title:    "Arduino: Trouver la longueur d'une chaîne"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes-vous déjà demandé comment mesurer la longueur d'une chaîne de caractères ? Si vous êtes un passionné d'Arduino, vous pourriez être intéressé à apprendre comment le faire grâce à la programmation en Arduino !

## Comment Faire

Pour mesurer la longueur d'une chaîne de caractères en Arduino, vous pouvez utiliser la fonction `strlen()`. Cette fonction prend en entrée la chaîne de caractères et renvoie un entier représentant sa longueur. Voici un exemple de code utilisant cette fonction :

```Arduino
char chaine[] = "Bonjour";
int longueur = strlen(chaine);
Serial.print("La longueur de la chaîne est de : ");
Serial.println(longueur);
```

Le code ci-dessus affichera la longueur de la chaîne "Bonjour" (7) sur le moniteur série.

## Plongée Profonde

Si vous vous demandez comment fonctionne la fonction `strlen()` en interne, voici une explication : elle parcourt la chaîne de caractères jusqu'à ce qu'elle trouve le caractère null '\0' qui marque la fin de la chaîne. Elle utilise ensuite un compteur pour suivre le nombre de caractères parcourus avant d'atteindre le caractère null et renvoie ce compteur en tant que longueur de la chaîne.

Il est important de noter que la fonction `strlen()` ne compte pas le caractère null dans la longueur de la chaîne. Par exemple, si vous déclarez une chaîne de caractères "Hello\0" où le caractère null est ajouté manuellement, la fonction `strlen()` renverra une longueur de 5 au lieu de 6.

## Voir Aussi

Pour en savoir plus sur la programmation en Arduino, consultez ces liens :

- [Documentation officielle Arduino](https://www.arduino.cc/en/Reference/HomePage)
- [Tutoriels Arduino pour débutants](https://www.arduino.cc/en/Tutorial/HomePage)
- [Communauté francophone d'Arduino](https://forum.arduino.cc/index.php?board=41.0)