---
title:                "Arduino: Majuscules d'une chaîne de caractères"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous vous demandez peut-être pourquoi quelqu'un voudrait capitaliser une chaîne de caractères dans un programme Arduino? La réponse est simple: cela peut être utile lorsque vous travaillez avec des données provenant de différentes sources et que vous voulez vous assurer que les noms sont formatés de manière cohérente.

## Comment faire

Heureusement, il est facile de capitaliser une chaîne de caractères en utilisant Arduino. Tout d'abord, vous devez inclure la bibliothèque "string.h" dans votre programme. Ensuite, utilisez la fonction "toUpperCase ()" pour convertir tous les caractères de la chaîne en majuscules.

```
Arduino #include <string.h>

void setup() {
    // initialisation du programme
}

void loop() {
    String nom = "arduino";
    nom.toUpperCase(); // convertir la chaîne en majuscules
    Serial.println(nom); // afficher "ARDUINO" dans le moniteur série
    delay(1000); // attendez une seconde avant de répéter la boucle
}
```

En utilisant cette méthode, vous pouvez capitaliser n'importe quelle chaîne de caractères dans votre programme Arduino.

## Plongée en profondeur

La fonction "toUpperCase ()" est disponible dans la bibliothèque "string.h" car elle est une fonction standard de C ++. Cette fonction modifie directement la chaîne de caractères d'origine, donc si vous avez besoin de conserver la chaîne de départ, vous devrez en faire une copie.

Il est également important de noter que la fonction "toUpperCase ()" ne fonctionne que pour les caractères ASCII. Si vous utilisez des caractères spéciaux ou des alphabets autres que latin, vous devrez utiliser des méthodes plus avancées pour capitaliser correctement votre chaîne de caractères.

## Voir aussi

Pour en savoir plus sur la manipulation de chaînes de caractères en Arduino, consultez ces ressources utiles:

- [Documentation officielle Arduino - Bibliothèque String](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Tutoriel sur les chaînes de caractères en Arduino](https://www.arduino.cc/en/Tutorial/StringComparisonOperators)
- [Exemples de projets utilisant la manipulation de chaînes en Arduino](https://create.arduino.cc/projecthub/projects/tags/string)
- [Forum Arduino - Discussions sur la manipulation de chaînes de caractères](https://forum.arduino.cc/index.php?board=2.0)

Maintenant que vous savez comment capitaliser une chaîne de caractères en Arduino, vous pouvez l'appliquer dans vos projets pour une plus grande cohérence et un meilleur traitement des données. Bonne programmation!