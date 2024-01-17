---
title:                "Écrire des tests"
html_title:           "Clojure: Écrire des tests"
simple_title:         "Écrire des tests"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-tests.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est que ça & pourquoi le faire?
Ecrire des tests est simplement le fait de créer du code qui vérifie que votre code fonctionne correctement. Les programmeurs le font pour s'assurer que leurs programmes fonctionnent comme prévu et pour éviter les bugs et les erreurs.

## Comment faire:
```Clojure 
(deftest test-addition
  (is (= (+ 2 3) 5))
  (is (= (+ 10 15) 25))
```

Dans cet exemple, nous déclarons un test appelé "test-addition" qui utilise la fonction "is" pour vérifier si l'addition de deux nombres est correcte. Ensuite, nous utilisons deux assertions pour s'assurer que le résultat de l'addition est égal à ce que nous attendons.

## Plongée dans les détails:
L'écriture de tests est une pratique courante dans la programmation depuis de nombreuses années. Avant l'avènement des frameworks de tests comme JUnit et NUnit, les programmeurs écrivaient leurs propres tests. Certains alternatives aux tests unitaires incluent les tests d'intégration et les tests fonctionnels. En ce qui concerne l'implémentation, Clojure fournit une librairie appelée "clojure.test" qui offre des fonctions utiles pour écrire des tests.

## Voir aussi:
- [Clojure.test library](https://clojure.github.io/clojure/clojure.test-api.html)
- [JUnit](https://junit.org/junit5/)
- [NUnit](https://nunit.org/)