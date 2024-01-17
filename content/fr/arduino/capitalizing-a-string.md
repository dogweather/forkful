---
title:                "Majusculation d'une chaîne de caractères"
html_title:           "Arduino: Majusculation d'une chaîne de caractères"
simple_title:         "Majusculation d'une chaîne de caractères"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Quoi et Pourquoi?

Capitaliser une chaîne de caractères signifie mettre en majuscule la première lettre de chaque mot dans une phrase donnée. Les programmeurs le font souvent pour améliorer la lisibilité et la présentation de leurs codes.

## Comment faire:

Utiliser la fonction `capitalize()` pour capitaliser une chaîne de caractères dans un code Arduino.

```
Arduino.prompt("Entrez une phrase à capitaliser ");
String phrase = Serial.readString(); // lire l'entrée de l'utilisateur
String phraseCapitalize = phrase.capitalize(); // utiliser la fonction capitalize()
Serial.print("La phrase capitale est : ");
Serial.print(phraseCapitalize);
```

## Plongez en profondeur:

Dans le passé, les programmeurs utilisaient souvent des boucles et des conditions pour capitaliser les chaînes de caractères, mais avec l'émergence de bibliothèques de fonctions prédéfinies, cela devient beaucoup plus simple. Il existe également d'autres façons de capitaliser une chaîne de caractères, telles que l'utilisation de fonctions de bibliothèque tierces ou la création de sa propre fonction. Dans le code ci-dessus, la fonction `capitalize()` utilise la règle de la langue anglaise pour capitaliser les lettres et ignore les articles et les prépositions.

## Voir aussi:

- [Documentation officielle d'Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/capitalize/)
- [Forum d'aide pour les débutants d'Arduino](https://forum.arduino.cc/index.php?topic=304561.0)
- [Tutoriel vidéo sur la capitalisation de chaîne de caractères dans Arduino](https://www.youtube.com/watch?v=Yn3eHxrMQEE)