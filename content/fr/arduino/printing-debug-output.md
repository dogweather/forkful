---
title:                "Afficher les sorties de débogage"
html_title:           "Arduino: Afficher les sorties de débogage"
simple_title:         "Afficher les sorties de débogage"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Imprimer des sorties de débogage est une méthode utilisée par les programmeurs pour afficher des informations sur les variables et le fonctionnement de leur code en cours d'exécution. Cela peut aider à identifier les erreurs et à comprendre le comportement du programme.

## Comment faire:

Voici un exemple de code montrant comment imprimer une sortie de débogage sur un arduino:

``` Arduino
int variable = 10;
Serial.print("La valeur de la variable est: ");
Serial.println(variable);
```

Cela affichera "La valeur de la variable est: 10" dans la console de débogage de l'IDE Arduino. Vous pouvez également utiliser la fonction ```Serial.println()``` pour afficher des messages personnalisés ou pour afficher plusieurs variables à la fois.

## Deep Dive:

L'impression de sorties de débogage a été couramment utilisée depuis les débuts de la programmation informatique pour aider les programmeurs à comprendre leur code et à le déboguer. Les alternatives à l'impression de sorties de débogage incluent l'utilisation de breakpoints, des outils de débogage visuel et la programmation d'une diode LED pour indiquer des états de fonctionnement.

L'implémentation de l'impression de sorties de débogage est assez simple, il vous suffit d'utiliser la fonction ```Serial.print()``` ou ```Serial.println()```, en veillant à inclure la bibliothèque ```<Serial.h>```. Vous pouvez également choisir la vitesse de transmission des données via le port série en utilisant la fonction ```Serial.begin()```.

## Voir aussi:

Pour en savoir plus sur l'impression de sorties de débogage sur Arduino, vous pouvez consulter ces sources:

- [Documentation officielle Arduino](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Article sur l'utilisation efficace de l'impression de débogage](https://barrgroup.com/embedded-systems/how-to/efficient-c-debugging-output)