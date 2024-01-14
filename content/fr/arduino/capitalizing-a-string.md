---
title:    "Arduino: Majuscule d'une chaîne de caractères"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes nouveau dans le monde de l'Arduino, vous vous demandez peut-être pourquoi vous voudriez capitaliser une chaîne de caractères. La réponse est simple : cela peut être utile lorsque vous travaillez avec des données textuelles qui doivent être mises en forme ou affichées correctement.

## Comment faire

Pour capitaliser une chaîne de caractères dans votre code Arduino, vous pouvez utiliser la fonction `toUpperCase()`. Dans l'exemple ci-dessous, nous créons une chaîne de caractères `message` et l'affichons à la fois en minuscules et en majuscules.

```Arduino
String message = "bonjour";
Serial.println(message); // affiche "bonjour"
message.toUpperCase();
Serial.println(message); // affiche "BONJOUR"
```

## Plongée en profondeur

La fonction `toUpperCase()` convertit tous les caractères de la chaîne en majuscules, tandis que la fonction `toLowerCase()` les convertit en minuscules. Mais que se passe-t-il si vous souhaitez capitaliser uniquement la première lettre d'une chaîne ? Pour cela, vous pouvez utiliser la fonction `charAt()` pour accéder à la première lettre et la convertir en majuscule à l'aide de la fonction `toUpperCase()`.

```Arduino
String message = "bonjour";
message.setCharAt(0, toUpperCase(message.charAt(0)));
Serial.println(message); // affiche "Bonjour"
```

Vous pouvez également utiliser ces fonctions dans une boucle pour capitaliser chaque mot d'une chaîne si nécessaire.

## Voir aussi

- [Documentation officielle de la fonction toUpperCase()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/touppercase/)
- [Documentation officielle de la fonction charAt()](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/charat/)
- [Autres fonctions de traitement de chaînes de caractères](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)