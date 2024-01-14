---
title:    "Arduino: La génération de nombres aléatoires"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Pourquoi générer des nombres aléatoires avec Arduino?

Si vous utilisez Arduino pour vos projets de bricolage, vous avez probablement déjà rencontré la nécessité de générer des nombres aléatoires. Les générateurs de nombres aléatoires sont souvent utilisés pour créer des effets visuels intéressants ou pour ajouter une touche d'imprévisibilité dans votre code. Dans cet article, nous allons explorer comment générer des nombres aléatoires avec Arduino.

## Comment générer des nombres aléatoires avec Arduino

Pour générer des nombres aléatoires avec Arduino, nous allons utiliser la fonction `random()`. Cette fonction prend deux paramètres, le nombre minimum et le nombre maximum, et renvoie un nombre aléatoire compris entre ces deux valeurs.

Prenons par exemple le code suivant:

```
Arduino :
// generate a random number between 1 and 10
int randomNumber = random(1, 11);
Serial.println(randomNumber);
```

Le code ci-dessus générera un nombre aléatoire compris entre 1 et 10 et l'affichera dans la console série.

## Plongez plus profondément

Si vous vous intéressez au fonctionnement interne des générateurs de nombres aléatoires, vous pouvez également jeter un coup d'œil à la fonction `randomSeed()`. Cette fonction initialise le générateur de nombres aléatoires en utilisant une valeur spécifique, créant ainsi une séquence de nombres aléatoires totalement unique.

Par exemple, vous pouvez utiliser des capteurs externes tels que des capteurs de température ou de lumière pour fournir une valeur de `randomSeed()` et créer des séquences de nombres aléatoires uniques en fonction des changements environnementaux.

# Voir aussi

- [Documentation officielle d'Arduino](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Tutoriel vidéo sur la génération de nombres aléatoires avec Arduino](https://www.youtube.com/watch?v=d-Y_EHLr7QY)
- [Article détaillé sur les générateurs de nombres aléatoires avec Arduino](https://create.arduino.cc/projecthub/bhavesh_dadukiya/generating-random-numbers-with-arduino-f581d0)