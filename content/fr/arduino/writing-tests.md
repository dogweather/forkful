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

# Qu'est-ce que c'est et pourquoi le faisons-nous?

Écrire des tests est une pratique courante pour les programmeurs afin de s'assurer que leur code fonctionne comme prévu. Il s'agit d'écrire un code supplémentaire pour vérifier que le code existant fonctionne correctement et de détecter les erreurs potentielles avant que le programme ne soit utilisé.

# Comment faire :

Pour écrire des tests dans Arduino, utilisez la bibliothèque standard ```UnitTest.h```. Il suffit d'inclure cette bibliothèque dans votre code et d'écrire des instructions pour tester les différentes parties de votre programme. Par exemple, vous pouvez créer un test pour vérifier qu'un capteur fonctionne correctement en comparant sa valeur avec une valeur attendue.

## Exemple de code :

```
#include <UnitTest.h>
int sensorValue = 0;
int expectedValue = 50; //valeur attendue du capteur

void setup(){
  //initialisation du capteur et des autres composants
}

void loop(){
  //lecture de la valeur du capteur
  sensorValue = analogRead(A0);
  //vérification avec la valeur attendue
  TEST_ASSERT_EQUAL(sensorValue, expectedValue);
}
```

## Résultat :

Si le test réussit, vous verrez un message "OK" dans le moniteur série. Sinon, il vous montrera sur quelle ligne le test a échoué et quelles étaient les valeurs réelles et attendues.

# Plongeons en profondeur :

L'écriture de tests est une pratique courante dans le développement logiciel depuis de nombreuses années. Elle permet de réduire le nombre d'erreurs dans le code et de faciliter la détection et le débogage des problèmes. Il existe également d'autres options pour écrire des tests tels que la bibliothèque CppUnit pour Arduino.

# Voir aussi :

Pour en savoir plus sur l'écriture de tests avec Arduino, vous pouvez consulter les ressources suivantes :


Maintenant que vous savez comment écrire des tests dans Arduino, allez-y et commencez à écrire du code robuste et sans bugs !