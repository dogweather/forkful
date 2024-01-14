---
title:    "Arduino: Ecrire sur l'erreur standard"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un programmeur Arduino passionné, vous avez probablement entendu parler d'écrire sur la sortie standard d'erreur. Mais pourquoi quelqu'un voudrait-il le faire ? Eh bien, la réponse est simple. L'écriture sur la sortie standard d'erreur peut être très utile pour déboguer votre code et trouver les erreurs plus rapidement.

## Comment faire

Il existe plusieurs façons d'écrire sur la sortie standard d'erreur en utilisant Arduino. Vous pouvez utiliser la fonction `Serial.print()` ou `Serial.println()` pour afficher des messages sur le moniteur série. Vous pouvez également utiliser la fonction `printf()` pour formater vos messages et y ajouter des variables.

Voici un exemple de code pour écrire sur la sortie standard d'erreur en utilisant `Serial.print()` :

```
Arduino.begin();

int num = 10;

Serial.print("La valeur de num est : ");
Serial.println(num); // cela ajoutera automatiquement un saut de ligne
```

Et voici un exemple utilisant `printf()` :

```
Arduino.begin();

int num1 = 20;
int num2 = 30;

printf("La valeur de num1 est : %d \n", num1);
printf("La valeur de num2 est : %d \n", num2);
```

L'exemple ci-dessus montre comment formater les messages avec différentes variables.

## Plongée en profondeur

Si vous voulez aller plus loin et comprendre le fonctionnement de l'écriture sur la sortie standard d'erreur, voici quelques informations supplémentaires. En utilisant la fonction `Serial.print()`, vous pouvez également spécifier un paramètre `BASE` pour spécifier dans quelle base numérique votre variable sera imprimée. Par exemple, `Serial.print(num, HEX)` imprimera la variable `num` en hexadécimal.

De plus, avec la fonction `printf()`, vous pouvez utiliser différents spécificateurs de conversion pour formater vos messages. Par exemple, `%s` pour une chaîne de caractères, `%f` pour un nombre à virgule flottante, `%c` pour un caractère, etc.

Maintenant que vous en savez plus sur l'écriture sur la sortie standard d'erreur, n'hésitez pas à l'exploiter pour améliorer votre processus de débogage.

## Voir aussi

- [Documentation Arduino officielle sur la sortie standard d'erreur](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)
- [Tutoriel vidéo sur l'utilisation de la sortie standard d'erreur en Arduino](https://www.youtube.com/watch?v=5d48X2zK0cs)
- [Article sur l'utilisation de la macro `DEBUG_PRINT` pour afficher des messages de débogage en Arduino](https://www.challengers101.com/arduino-tutorial/debug-print-arduino-tutorial/)