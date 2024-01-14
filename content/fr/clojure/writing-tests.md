---
title:    "Clojure: Ecrire des tests"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi

L'écriture de tests est une étape importante dans le développement de logiciels. Elle permet de s'assurer que le code fonctionne correctement et de détecter d'éventuelles erreurs plus rapidement. En outre, elle facilite la maintenance du code à long terme.

# Comment faire

Pour écrire des tests en Clojure, il existe plusieurs outils disponibles tels que le framework de test intégré clojure.test ou encore le plugin CIDER pour l'IDE Emacs. Suivez ces étapes pour écrire des tests efficaces :

1. Développez le code que vous souhaitez tester.
2. Importez le framework de test ou le plugin CIDER.
3. Ecrivez les tests en utilisant des assertions pour vérifier les résultats attendus.
4. Exécutez les tests et vérifiez les résultats.

Voici un exemple de code en Clojure avec un test utilisant clojure.test :

```Clojure
(ns mon-projet.tests
  (:require [clojure.test :refer :all]
            [mon-projet.code :refer :all]))

(deftest test-addition
  (testing "Vérifier que l'addition fonctionne correctement"
    (is (= (addition 2 3) 5))
    (is (= (addition 0 0) 0))
    (is (= (addition -1 1) 0))))
```

La sortie attendue serait la suivante :

```
Testing mon-projet.tests
Ran 1 tests containing 3 assertions.
0 failures, 0 errors.
```

# Approfondissement

Il est important de noter que les tests doivent être écrits de manière à être automatisés, c'est-à-dire qu'ils doivent pouvoir être exécutés de manière répétée sans nécessiter d'intervention manuelle. Cela permet de gagner du temps et de garantir la fiabilité des tests.

De plus, les tests doivent être spécifiques et couvrir tous les cas possibles. Par exemple, pour l'addition, il serait judicieux de tester également la division par zéro pour s'assurer qu'elle renvoie bien une erreur.

Enfin, il est conseillé de suivre les principes du TDD (Test Driven Development) en écrivant les tests en premier, avant même le code. Cela permet de mieux cerner les fonctionnalités attendues et de s'assurer que le code répond bien à ces attentes.

# Voir aussi

- [Documentation officielle sur clojure.test] (https://clojure.org/guides/testing)
- [Guide sur le TDD en Clojure] (https://blog.cleancoder.com/uncle-bob/2017/10/03/Testability.html)
- [Tutoriel sur l'utilisation du plugin CIDER pour écrire des tests] (https://pedestal.io/guides/testing-with-emacs/)