---
title:                "Arduino: Majuscule d'une chaîne de caractères"
simple_title:         "Majuscule d'une chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation Arduino, il est parfois nécessaire de capitaliser une chaîne de caractères. Cela peut être utile pour l'affichage de données utilisateur ou pour la manipulation de données dans un projet.

## Comment faire

Pour capitaliser une chaîne de caractères, vous aurez besoin de la fonction `capitalize()`. Voici un exemple de code pour comprendre comment utiliser cette fonction :

```Arduino
String str = "exemplo";
str.capitalize();
Serial.println(str); // Affiche "Exemplo"
```

La chaîne de caractères "exemplo" est d'abord stockée dans la variable `str`, puis la fonction `capitalize()` est utilisée pour la capitaliser. Enfin, la chaîne modifiée est affichée sur le moniteur série. Le résultat sera "Exemplo".

## Plongée en profondeur

Maintenant que vous savez comment utiliser la fonction `capitalize()`, voyons un peu plus en détails comment elle fonctionne. Cette fonction utilise la table ASCII pour déterminer si un caractère est en majuscule ou en minuscule. Les caractères compris entre 97 et 122 (a à z) sont convertis en majuscules en soustrayant 32 à leur code ASCII. Cette opération est effectuée pour chaque caractère de la chaîne, ce qui explique pourquoi la fonction `capitalize()` ne fonctionne qu'avec des lettres.

## Voir aussi

- [Documentation officielle sur la fonction `capitalize()`](https://www.arduino.cc/reference/en/language/functions/strings/capitalize/)
- [Guide complet sur la manipulation de chaînes de caractères en Arduino](https://www.programmingelectronics.com/ultimate-guide-arduino-strings/)
- [Exemples de projets utilisant la fonction `capitalize()`](https://create.arduino.cc/projecthub/projects/tags/capitalize/string)

Merci d'avoir lu cet article ! Nous espérons qu'il vous a été utile dans vos projets Arduino. N'hésitez pas à consulter les liens ci-dessus pour en apprendre davantage sur la capitalisation des chaînes de caractères en Arduino.