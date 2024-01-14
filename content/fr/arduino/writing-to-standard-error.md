---
title:    "Arduino: Écriture vers l'erreur standard"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation Arduino, il est parfois nécessaire d'écrire un message d'erreur quelque part pour un débogage facile. C'est pourquoi il existe une fonction utile appelée "write to standard error".

## Comment faire

Pour utiliser cette fonction, on a besoin de deux éléments : un message d'erreur à envoyer et la fonction "Serial". Voici un exemple de code montrant comment écrire le message d'erreur "Erreur de détection de capteur" :

```Arduino
Serial.println("Erreur de détection de capteur");
```

Le résultat de cette instruction serait l'affichage du message "Erreur de détection de capteur" dans la fenêtre de communication série de votre logiciel de programmation Arduino.

## Profonde plongée

Il est important de noter que la fonction "write to standard error" n'est pas seulement utile pour les messages d'erreur, mais aussi pour les avertissements et les informations de débogage.

Il est également possible d'envoyer des messages d'erreur à la console uniquement lorsque le programme est en mode de débogage. Pour cela, on peut utiliser la condition "if" suivante :

```Arduino
if (DEBUG) {
  Serial.println("Message d'erreur...");
}
```

Cette condition ne sera vraie que si la variable "DEBUG" est définie dans votre programme.

## Voir aussi

- [Documentation officielle sur la fonction "Serial"](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Tutoriel pour débutants sur la communication série avec Arduino](https://www.circuitbasics.com/arduino-serial-communication-basics/)