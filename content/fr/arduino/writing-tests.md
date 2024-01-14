---
title:    "Arduino: Écriture de tests"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Pourquoi écrire des tests en programmation Arduino ?

Écrire des tests lors de la programmation Arduino peut sembler fastidieux et peu utile, mais cela peut en fait être très bénéfique pour votre projet. Les tests permettent de s'assurer que le code fonctionne correctement et de détecter d'éventuels bugs plus rapidement. De plus, cela facilite la maintenance du code à long terme.

## Comment écrire des tests en Arduino ?

Il existe différentes manières de réaliser des tests en Arduino. Voici un exemple de code pour tester une fonction simple qui additionne deux nombres :

```Arduino
int add(int a, int b) {
  return a + b;
}

void testAddition() {
  int result = add(3, 5);
  if (result != 8) {
    Serial.println("L'addition a échoué !");
  }
  else {
    Serial.println("L'addition a réussi !");
  }
}
```

Dans cet exemple, nous créons une fonction ```add``` qui prend deux entiers en paramètres et les additionne. Ensuite, nous créons une fonction ```testAddition``` qui utilise la fonction ```add``` pour vérifier si l'addition de 3 et 5 donne bien le résultat attendu, à savoir 8. En fonction du résultat, nous imprimons un message de réussite ou d'échec.

Avec ce type de tests, vous pouvez également vérifier les différentes possibilités de valeurs en entrée, comme des nombres négatifs ou des nombres à virgule.

## Approfondissement sur l'écriture de tests en Arduino

Il existe différentes méthodes pour écrire des tests en Arduino, notamment l'utilisation de bibliothèques dédiées telles que la bibliothèque "ArduinoUnit". Ces bibliothèques offrent des fonctionnalités plus avancées pour le test de code, telles que la possibilité d'effectuer des tests unitaires ou des tests de performances.

Il est également important de noter que les tests doivent être écrits au fur et à mesure de l'écriture du code, afin de ne pas accumuler trop de tests à la fin. Vous pouvez également utiliser des outils de couverture de code pour mesurer la quantité de code testée par les tests que vous avez écrits.

# Voir aussi

- Tutoriel Arduino : Écrire et exécuter des tests unitaires avec ArduinoUnit : https://create.arduino.cc/projecthub/TheGadgetKeeper/unit-testing-on-arduino-with-arduinounit-b968b0
- Utiliser la bibliothèque ArduinoUnit pour écrire des tests de performance : https://forum.arduino.cc/t/performance-testing-using-arduinounit-library/389022
- Les avantages des tests unitaires en Arduino : https://electronics.stackexchange.com/questions/91986/advantages-of-unit-testing-in-arduino