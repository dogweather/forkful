---
title:                "Arduino: Afficher la sortie de débogage"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous codez avec Arduino pour vos projets électroniques, il est crucial de pouvoir comprendre ce qui se passe dans votre code. C'est ici que l'impression de sortie de débogage intervient. Cela vous permet d'afficher des informations utiles sur votre code en temps réel, ce qui vous aide à identifier les erreurs et à améliorer vos performances.

## Comment faire

Pour imprimer des informations de débogage sur votre code Arduino, il vous suffit d'utiliser la fonction `Serial.print()`. Cette fonction prend en argument une valeur, une chaîne de caractères ou une variable et l'imprime sur le moniteur série. Voici un exemple de code :

```Arduino
int x = 10;
Serial.print("La valeur de x est : ");
Serial.print(x);
```

En téléversant ce code sur votre carte Arduino et en ouvrant le moniteur série, vous verrez s'afficher : "La valeur de x est : 10". Vous pouvez utiliser également la fonction `Serial.println()` pour ajouter un saut de ligne après l'impression de la valeur.

## Plongée en profondeur

Pour une utilisation plus avancée de la fonction `Serial.print()`, vous pouvez spécifier la base numérique de la valeur à imprimer. Par exemple, en utilisant `Serial.print(x, BIN)`, la valeur sera imprimée en binaire. Vous pouvez également utiliser `Serial.printf()` pour formater l'impression selon un modèle spécifique.

Enfin, il est important de procéder à l'impression de débogage avec parcimonie, en n'imprimant que les informations pertinentes. Cela vous aidera à garder votre code propre et à éviter de surcharger le moniteur série.

## Voir aussi

- [Documentation officielle de la fonction Serial.print()](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Tuto Arduino : l'impression de débogage](https://www.arduino.cc/en/Tutorial/SerialPrint)
- [Comment déboguer votre code Arduino avec l'impression de débogage](https://www.makerguides.com/arduino-serial-print-debugging/)