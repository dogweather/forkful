---
title:                "Écriture de tests"
html_title:           "Arduino: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un fan d'Arduino, vous savez probablement déjà que les tests sont importants pour assurer le bon fonctionnement de votre code. Mais pour ceux qui ne sont pas familiers avec le concept, voici pourquoi il est important d'écrire des tests pour vos projets Arduino.

Les tests vous aident à identifier et à corriger les erreurs dans votre code avant même de le télécharger sur votre carte Arduino. Cela vous fait gagner du temps et vous évite des frustrations à essayer de comprendre pourquoi votre projet ne fonctionne pas correctement.

## Comment faire

Écrire des tests pour vos projets Arduino n'est pas compliqué et peut vous épargner beaucoup de maux de tête à long terme. Voici un exemple simple de code Arduino avec des tests intégrés:

```arduino
int LED = 13;  // Broche de la LED

// Fonction qui allume la LED pour 1 seconde
void turnOnLed(){
  digitalWrite(LED, HIGH);
  delay(1000);
  digitalWrite(LED, LOW);
}

void setup() {
  pinMode(LED, OUTPUT);  // Définir la broche de la LED comme sortie
}

void loop() {
  turnOnLed(); // Appel de notre fonction pour allumer la LED
}
```

Pour tester cette fonction, nous pouvons ajouter une autre fonction qui vérifie si la LED a été allumée pour la bonne durée. Voici un exemple de test en utilisant la bibliothèque [Arduino Unit](https://github.com/mmurdoch/arduinounit):

```arduino
unittest(LED_test) {  // Définition du test
  turnOnLed();  // Appel de la fonction à tester
  assertTrue(digitalRead(LED), "La LED n'a pas été allumée");  // Vérification de l'état de la broche de la LED
  delay(1000);  // Attente de 1 seconde
  assertFalse(digitalRead(LED), "La LED n'a pas été éteinte après la durée spécifiée");  // Vérification de l'état de la broche de la LED après la durée spécifiée
}
```

En utilisant des tests comme celui-ci, vous pouvez vérifier le bon fonctionnement de votre code en toute confiance, en sachant que vous avez pris en compte les différentes conditions possibles.

## Plongée en profondeur

Écrire des tests pour votre code Arduino peut sembler fastidieux, mais cela en vaut la peine à long terme. En utilisant des outils comme la bibliothèque Arduino Unit, vous pouvez tester des fonctions plus complexes avec plusieurs cas de test.

Il est également important de noter que les tests peuvent aider à détecter les bugs dans votre code lorsque vous apportez des modifications ou ajoutez de nouvelles fonctionnalités. En plus d'assurer un code de meilleure qualité, les tests peuvent également vous faire gagner du temps à long terme en réduisant le temps passé à déboguer des erreurs.

## Voir aussi

- [Documentation Arduino Unit](https://github.com/mmurdoch/arduinounit)
- [Guide sur les tests pour les projets Arduino](https://www.martyncurrey.com/testing-projects-with-arduino-unit/)