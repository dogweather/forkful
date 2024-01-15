---
title:                "L'écriture vers la sortie d'erreur standard"
html_title:           "Arduino: L'écriture vers la sortie d'erreur standard"
simple_title:         "L'écriture vers la sortie d'erreur standard"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous avez déjà écrit du code pour Arduino, vous savez probablement qu'il est important d'avoir un moyen de déboguer ou de comprendre ce qui se passe lors de l'exécution de votre programme. C'est là que l'écriture vers l'erreur standard entre en jeu. Elle permet d'afficher des messages d'erreur ou des informations de débogage sur la console série, ce qui peut être très utile lors du développement de votre projet.

## Comment faire

Ecrire vers l'erreur standard en utilisant Arduino est assez simple. Il vous suffit d'utiliser la fonction ```Serial.println()``` en spécifiant "Erreur" comme premier argument. Voici un exemple de code qui affiche un message d'erreur sur la console série :

```
Arduino
void setup() {
  // Ouvre la communication série à 9600 baud
  Serial.begin(9600);
}

void loop() {
  // Ecrit "Erreur : une erreur s'est produite !" vers l'erreur standard
  Serial.println("Erreur : une erreur s'est produite !");
  delay(1000);
}
```

Lorsque vous téléversez ce code sur votre carte Arduino et que vous ouvrez le moniteur série, vous devriez voir le message d'erreur s'afficher toutes les secondes.

## Plongée en profondeur

Il est important de noter que l'écriture vers l'erreur standard n'est pas seulement utile pour afficher des messages d'erreur. Vous pouvez également l'utiliser pour afficher des informations de débogage telles que des valeurs de variables, des états de capteurs ou toute autre information qui peut vous aider à comprendre ce qui se passe dans votre code.

De plus, vous pouvez également utiliser la fonction ```Serial.print()``` pour écrire sur l'erreur standard. Cette fonction vous permet de spécifier différents types de données en utilisant des caractères de formatage tels que %d pour les entiers, %f pour les nombres à virgule flottante, etc. Voici un exemple de code qui utilise la fonction Serial.print() pour afficher la valeur d'une variable sur la console série :

```
Arduino
void setup() {
  // Ouvre la communication série à 9600 baud
  Serial.begin(9600);
}

void loop() {
  int compteur = 0; // Variable compteur initialisée à 0
  // Ecrit la valeur de compteur sur la console série
  Serial.print("Compteur : %d", compteur);
  compteur++;
  delay(1000);
}
```

Lorsque vous téléversez ce code et que vous ouvrez le moniteur série, vous devriez voir le compteur s'incrémenter à chaque seconde.

## Voir aussi

Maintenant que vous savez comment écrire vers l'erreur standard en utilisant Arduino, voici quelques autres liens utiles pour en apprendre plus sur le sujet :

- La documentation officielle d'Arduino sur la communication série : https://www.arduino.cc/reference/en/language/functions/communication/serial/
- Une vidéo tutoriel sur l'utilisation de la communication série avec Arduino : https://www.youtube.com/watch?v=oM4-PDrb2iU
- Un article sur l'écriture vers l'erreur standard en C : https://www.geeksforgeeks.org/stderr-vs-stdout-in-c/