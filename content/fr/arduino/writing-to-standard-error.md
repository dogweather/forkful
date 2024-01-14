---
title:                "Arduino: Écriture vers l'erreur standard"
simple_title:         "Écriture vers l'erreur standard"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un développeur Arduino, vous savez déjà que lorsque vous programmez, il est important d'écrire à la sortie standard pour déboguer votre code. Cela vous permet de voir les erreurs et les messages de débogage en temps réel afin de faciliter la résolution des problèmes. Dans cet article, nous allons vous montrer comment écrire à la sortie standard de votre Arduino pour un débogage efficace.

## Comment faire 

Pour écrire à la sortie standard de votre Arduino, vous devrez utiliser la fonction `Serial.println()`. Cette fonction prend votre message en tant que paramètre et l'envoie à la sortie standard. Par exemple, si vous voulez afficher le texte "Hello World" à la sortie standard, vous écririez le code suivant : 

```Arduino
Serial.println("Hello World");
```

Lorsque vous téléversez votre code sur votre Arduino et ouvrez le moniteur série (Serial Monitor), vous devriez voir le message "Hello World" s'afficher. Vous pouvez également utiliser la fonction `Serial.print()` pour afficher des messages sans retour à la ligne après chaque message. 

## Plongée en profondeur 

Il est important de noter que l'utilisation de la fonction `Serial.println()` peut affecter les performances de votre code, surtout si vous l'utilisez à plusieurs reprises dans une boucle. Pour éviter cela, vous pouvez utiliser la fonction `Serial.flush()` pour vider le tampon de la sortie standard avant d'entrer dans une boucle. Cela évitera de surcharger votre programme avec trop de messages de débogage.

De plus, si vous voulez être plus précis lors du débogage, vous pouvez écrire à la sortie d'erreur (standard error) plutôt qu'à la sortie standard. Pour ce faire, vous utiliserez la fonction `Serial.print()` suivie du numéro de la sortie que vous voulez utiliser. Par exemple, si vous voulez écrire à la sortie d'erreur 1, vous écririez le code suivant :

```Arduino
Serial.print(1);
```

Cela enverra le message "1" à la sortie d'erreur plutôt qu'à la sortie standard. Vous pouvez également utiliser la fonction `Serial.write()` pour écrire directement des octets à la sortie standard ou d'erreur.

## Voir aussi 

Pour en savoir plus sur l'écriture à la sortie standard et à la sortie d'erreur de votre Arduino, voici quelques liens utiles :

- [Documentation officielle sur la classe Serial](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Vidéo "Serial Communication with Arduino"](https://www.youtube.com/watch?v=uf_CpDwU0Zg)
- [Article "Debugging Arduino Code with the Serial Port"](https://learn.sparkfun.com/tutorials/debugging-with-the-serial-ported--using-the-serial-ports/debugging-arduino-code-with-the-serial-port)

Maintenant que vous savez comment écrire à la sortie standard et à la sortie d'erreur de votre Arduino, vous pouvez facilement déboguer votre code et éviter les erreurs. Bon codage !