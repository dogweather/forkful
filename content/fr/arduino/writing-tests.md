---
title:                "Arduino: Écrire des tests"
programming_language: "Arduino"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire des tests pour votre code Arduino peut sembler fastidieux et inutile, mais c'est en fait une étape cruciale pour s'assurer que votre projet fonctionne correctement. Les tests vous permettent de détecter et de corriger les erreurs avant même qu'elles ne se manifestent dans votre projet.

## Comment faire

Pour écrire des tests pour votre code Arduino, vous devrez utiliser une bibliothèque de test telle que ArduinoUnit ou Unity. Ces bibliothèques vous permettent de créer des cas de test et de vérifier les résultats attendus.

Voici un exemple de code utilisant ArduinoUnit pour tester une fonction de calcul de la température :

```Arduino
#include <ArduinoUnit.h>

int calculateTemperature(int rawVoltage) {
  return ((rawVoltage * 5) / 1024) * 100;
}

test(calculateTemperature) {
  int expected = 400;
  int actual = calculateTemperature(818);
  assertEqual(expected, actual);
}

unittest_main()
```

Dans cet exemple, nous créons une fonction de calcul de la température qui prend en paramètre une valeur brute de tension et renvoie la température en degrés Celsius. Dans notre cas de test, nous nous attendons à ce que la fonction renvoie 400 lorsque la valeur brute de tension est de 818. Nous utilisons ensuite la fonction "assertEqual" pour vérifier que la valeur renvoyée est bien de 400.

En exécutant ce test, nous pouvons nous assurer que notre fonction de calcul de la température fonctionne correctement et qu'elle renvoie les résultats attendus.

Vous pouvez également utiliser des bibliothèques de test pour vérifier le fonctionnement de vos composants physiques, tels que les capteurs ou les actionneurs. Cela peut vous aider à détecter rapidement les problèmes de connexion ou de compatibilité entre vos composants.

## Plongeons plus profondément

Pour écrire des tests de manière plus efficace, il est important de comprendre les principes du Test Driven Development (TDD). Ce concept consiste à écrire les tests avant même d'écrire le code, en se concentrant sur le résultat attendu plutôt que sur l'implémentation.

En utilisant TDD, vous pouvez vous assurer que votre code est bien testable et que les tests que vous écrivez sont pertinents et efficaces. En plus de cela, TDD peut vous aider à maintenir un code plus propre et mieux structuré.

## Voir aussi

- Inspecteur de code Arduino : <https://www.arduino.cc/en/pmwiki.php?n=Reference/CodeInspector>
- ArduinoUnit : <https://github.com/mmurdoch/arduinounit>
- Unity : <https://github.com/ThrowTheSwitch/Unity>