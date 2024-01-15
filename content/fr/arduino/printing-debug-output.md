---
title:                "Imprimer la sortie de débogage"
html_title:           "Arduino: Imprimer la sortie de débogage"
simple_title:         "Imprimer la sortie de débogage"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà programmé sur Arduino, vous savez probablement que le débogage peut être un véritable casse-tête. L'impression de messages de débogage peut vous aider à comprendre le fonctionnement de votre code et à identifier les erreurs plus facilement.

## Comment faire

Pour imprimer des messages de débogage dans votre code Arduino, vous pouvez utiliser la fonction `Serial.print()`. Elle permet d'afficher une valeur ou une chaîne de caractères. Vous pouvez également utiliser `Serial.println()` pour afficher un retour à la ligne après chaque message de débogage.

```
//Imprimer une valeur
int valeur = 10;
Serial.print("La valeur est égale à : ");
Serial.print(valeur);

//Imprimer une chaîne de caractères
String texte = "Bonjour tout le monde !";
Serial.print("La chaîne de caractères est : ");
Serial.print(texte);

//Imprimer avec retour à la ligne
int temp = 25;
Serial.print("La température est de : ");
Serial.print(temp);
Serial.println(" degrés Celsius.");
```

Dans l'exemple ci-dessus, vous remarquerez que le texte à imprimer est placé entre guillemets. Cela indique à l'Arduino qu'il s'agit d'une chaîne de caractères et non d'une variable.

## Plongée en profondeur

En plus de la fonction `Serial.print()`, l'Arduino a d'autres fonctions pour l'impression de messages de débogage. `Serial.write()` permet d'écrire des données binaires sur le port série et `Serial.printf()` permet d'imprimer des messages avec un format spécifié. De plus, avec la librairie `SoftwareSerial`, vous pouvez créer un port série virtuel et imprimer des messages sur celui-ci.

Il est important de noter que l'utilisation excessive de l'impression de messages de débogage peut ralentir l'exécution de votre code et prendre de la place dans la mémoire de votre Arduino. N'hésitez pas à commenter ou supprimer vos messages de débogage une fois que vous avez corrigé vos erreurs.

## Voir aussi

- [Guide de l'utilisation de Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Tutoriel sur l'utilisation de Serial.print()](https://www.youtube.com/watch?v=hEAkzG1g8iw)
- [Guide de l'utilisation de SoftwareSerial](https://www.arduino.cc/en/Reference/softwareSerial)
- [Tutoriel sur l'utilisation de SoftwareSerial](https://www.youtube.com/watch?v=9_1f1CgFjz0)