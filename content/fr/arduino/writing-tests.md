---
title:    "Arduino: Écrire des tests"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi
Ecrire des tests peut sembler fastidieux et prendre du temps, mais c'est en réalité une étape très importante dans le processus de développement avec Arduino. Non seulement cela garantit que votre code fonctionne correctement, mais cela vous aide également à détecter et à résoudre rapidement les erreurs potentielles.

## Comment faire
Ecrire des tests avec Arduino n'est pas aussi difficile qu'il n'y parait. Il vous suffit d'utiliser la fonction ```test()``` pour définir vos tests, puis d'utiliser ```assert()``` pour vérifier si les résultats correspondent à ceux que vous attendez. Voici un exemple simple de test pour une fonction qui multiplie deux nombres :

```Arduino
int multiplication(int a, int b) {
  return a * b;
}

void testMultiplication() {
  assert(multiplication(2, 3) == 6); // Vérifie si le résultat est égal à 6
  assert(multiplication(5, 5) == 25); // Vérifie si le résultat est égal à 25
}

void setup() {
  // Lancez votre test ici
  testMultiplication();
}

void loop() {
  // Code principal de votre programme
}
```

Lors de l'exécution de ce code, si l'un de vos tests échoue, vous saurez immédiatement quel test en question a échoué et pour quelles raisons.

## Deep Dive
Pour écrire des tests plus complexes, vous pouvez utiliser des bibliothèques spécifiques telles que ```ArduinoUnit``` ou ```Unity``` pour tester des fonctions plus avancées telles que les fonctions temporisées ou les fonctions impliquant des composants externes. Ces bibliothèques offrent également une meilleure gestion des erreurs et des options de test plus avancées.

Il est également important de noter que les tests doivent être écrits régulièrement tout au long du processus de développement, en particulier lorsque vous ajoutez de nouvelles fonctionnalités à votre code. Cela garantit que votre code reste fonctionnel et facile à maintenir à mesure que vous ajoutez de nouvelles modifications.

## Voir aussi
- [Tutoriel d'Arduino sur l'écriture de tests] (https://www.arduino.cc/en/Guide/ArduinoUnitTesting)
- [GitHub - ArduinoUnit] (https://github.com/mmurdoch/arduinounit)
- [GitHub - Unity] (https://github.com/ThrowTheSwitch/Unity)