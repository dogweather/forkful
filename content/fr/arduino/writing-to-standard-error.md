---
title:                "Écrire sur les erreurs standard"
html_title:           "Arduino: Écrire sur les erreurs standard"
simple_title:         "Écrire sur les erreurs standard"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'écriture sur la sortie standard d'erreur est une technique de programmation qui permet aux programmeurs de signaler des erreurs ou des avertissements pendant l'exécution de leur programme. Cela permet de déboguer plus facilement les problèmes et d'améliorer la qualité du code.

## Comment faire:
Voici un exemple de code en utilisant l'écriture sur la sortie standard d'erreur dans Arduino :

```
void setup() {
  Serial.begin(9600); // Initialise la communication série à 9600 bauds
  Serial.println("Début du programme"); // Écrit un message sur la sortie standard d'erreur
}

void loop() {
  int temp = analogRead(A0); // Lit la valeur analogique du capteur connecté à la broche A0
  if (temp > 500) { // Vérifie si la température est supérieure à 50 degrés Celsius
    Serial.println("Attention - température élevée!"); // Écrit un avertissement sur la sortie standard d'erreur
  }
  delay(1000); // Attend une seconde avant de continuer
}
```

Voici à quoi ressemblerait la sortie sur le moniteur série :

```
Début du programme
Attention - température élevée!
Attention - température élevée!
Attention - température élevée!
...
```

Comme vous pouvez le voir, le message d'avertissement est imprimé à chaque fois que la température dépasse 50 degrés Celsius. Cela peut être utile pour surveiller les capteurs ou pour gérer les erreurs dans le code.

## Plongée profonde:
L'écriture sur la sortie standard d'erreur a longtemps été une pratique courante dans la programmation, en particulier dans les langages de bas niveau tels que le C et le C++. Elle est également utilisée dans les microcontrôleurs tels que Arduino pour déboguer les programmes.

Une alternative à l'écriture sur la sortie standard d'erreur est l'utilisation de LED ou de moniteurs LCD pour afficher des messages d'erreur en temps réel. Cependant, cela peut prendre plus de temps à mettre en place et nécessite du matériel supplémentaire.

Pour écrire sur la sortie standard d'erreur dans Arduino, il suffit d'utiliser la fonction ```Serial.println()```. Cela envoie une chaîne de caractères à la sortie série, qui peut être lue en utilisant un moniteur série.

## Voir aussi:
Pour en savoir plus sur l'utilisation de l'écriture sur la sortie standard d'erreur dans Arduino, vous pouvez consulter la documentation officielle sur la communication série (https://www.arduino.cc/reference/en/language/functions/communication/serial/). Vous pouvez également consulter des tutoriels en ligne pour des exemples plus avancés d'utilisation de cette technique de programmation.