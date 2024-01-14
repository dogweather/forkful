---
title:                "Arduino: Affichage des sorties de débogage"
simple_title:         "Affichage des sorties de débogage"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

# Pourquoi 
Pourquoi quelqu'un se lancerait-il dans l'impression de messages de débogage ? 

Lorsque vous travaillez sur un projet Arduino, il est souvent utile de pouvoir suivre votre code et de comprendre ce qui se passe en utilisant des messages de débogage. Ces messages peuvent vous aider à identifier et à résoudre les erreurs dans votre code, ce qui est essentiel pour un fonctionnement efficace de votre projet.

# Comment Faire 
Pour imprimer des messages de débogage dans votre code Arduino, vous pouvez utiliser la fonction `Serial.print()`. Cette fonction prend en paramètre la valeur que vous souhaitez afficher et l'envoie à l'ordinateur par le port série. Vous pouvez également utiliser `Serial.println()` pour ajouter un retour à la ligne à la fin de votre message.

```
ArduinoSerial.print("Valeur : ");
ArduinoSerial.println(valeur);
```

Dans l'exemple ci-dessus, nous utilisons `Serial.print()` pour afficher le texte "Valeur : " et `Serial.println()` pour afficher la valeur de la variable `valeur`. Lorsque vous téléversez votre code sur votre carte Arduino, vous pourrez alors voir ces messages de débogage dans la console série de l'IDE Arduino.

# Plongée en Profondeur 
Lorsque vous utilisez des messages de débogage, il est important de garder à l'esprit quelques bonnes pratiques. Tout d'abord, évitez d'imprimer trop de messages de débogage, cela peut ralentir votre code et utiliser trop de mémoire. Ensuite, vous pouvez utiliser `Serial.begin()` pour initialiser la communication série avec un débit spécifique, ceci peut être utile si vous rencontrez des problèmes avec des connexions plus lentes. De plus, vous pouvez utiliser des commandes conditionnelles pour n'afficher que certains messages de débogage lorsque vous le souhaitez.

# Voir Aussi
- Tutoriel officiel d'Arduino sur la communication série : https://www.arduino.cc/en/Tutorial/Serial
- Article sur les bonnes pratiques pour l'utilisation des messages de débogage : https://www.instructables.com/id/Debugging-on-Arduino-great-practices
- Bibliothèque de débogage Arduino SerialDebug : https://github.com/JoaoLopesF/Arduino-SerialDebug