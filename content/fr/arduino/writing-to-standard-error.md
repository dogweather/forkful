---
title:                "Arduino: Ecrire sur l'erreur standard"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous programmez avec Arduino, il peut être utile d'écrire vers la sortie d'erreur pour déboguer votre code et trouver des erreurs plus facilement. Cela peut également vous aider à comprendre le fonctionnement interne de votre code.

## Comment faire

Écrire vers la sortie d'erreur en utilisant Arduino est très simple. Vous pouvez le faire en utilisant la fonction ```Serial.println()``` et en passant votre message en paramètre. Par exemple :

```Arduino
Serial.println("Erreur de lecture du capteur de température !");
```

Cela enverra le message "Erreur de lecture du capteur de température !" vers la sortie d'erreur, qui peut être consultée sur l'interface de communication série de votre Arduino.

Pour voir ce message, vous devrez ouvrir l'interface de communication série en cliquant sur l'icône en forme de loupe en haut à droite de l'IDE Arduino.

## Plongée en profondeur

En écrivant vers la sortie d'erreur, vous pouvez également spécifier le niveau de gravité de votre message en utilisant des constantes telles que ```INFO```, ```WARNING``` ou ```ERROR```. Cela peut être utile pour identifier rapidement le type d'erreur sans avoir à lire le message complet.

De plus, vous pouvez également écrire vers la sortie d'erreur en utilisant d'autres méthodes au lieu de ```Serial.println()```, par exemple en utilisant la bibliothèque ```SoftwareSerial``` pour envoyer des données vers une autre broche série.

## Voir aussi

- [Documentation officielle sur l'utilisation de la sortie d'erreur avec Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/println/) 
- [Article sur la manipulation des erreurs en programmation Arduino](https://www.maketecheasier.com/error-handling-in-arduino/) 
- [Forum de la communauté Arduino pour des discussions sur l'utilisation de la sortie d'erreur](https://forum.arduino.cc/)