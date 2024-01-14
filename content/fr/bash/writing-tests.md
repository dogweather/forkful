---
title:                "Bash: Écrire des tests"
programming_language: "Bash"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi écrire des tests en Bash

Ecrire des tests pour votre code Bash peut sembler fastidieux, mais cela peut être très bénéfique à long terme. Les tests vous aident à détecter et à corriger les erreurs de manière proactive, ce qui peut vous faire économiser du temps et des maux de tête dans le futur. De plus, ils vous permettent de vous assurer que votre code fonctionne correctement avant de le déployer en production.

## Comment faire

Pour écrire des tests en Bash, vous pouvez utiliser l'utilitaire de test intégré appelé `bashunit`. Cet outil vous permet de créer des tests automatisés pour votre code sans avoir à installer d'autres dépendances.

Voici un exemple simple de test de la fonction `addition` qui prend deux paramètres et renvoie leur somme :

```Bash
#!/bin/bash

# Importez la bibliothèque bashunit
source bashunit.sh

# Définissez votre fonction à tester
addition() {
  echo $(( $1 + $2 ))
}

# Définissez votre cas de test en utilisant la syntaxe "assert equal"
test_addition() {
  assert_equal $(addition 2 3) 5 "La somme de 2 et 3 devrait être égale à 5"
  assert_equal $(addition -1 5) 4 "La somme de -1 et 5 devrait être égale à 4"
}

# Lancez le test en utilisant la fonction "run_test"
run_test test_addition
```

En exécutant ce script, vous devriez obtenir une sortie similaire à celle-ci :

```
# test_addition
✔ La somme de 2 et 3 devrait être égale à 5
✔ La somme de -1 et 5 devrait être égale à 4
Vérifications exécutées : 2, réussites : 2
```

N'hésitez pas à expérimenter avec d'autres fonctions de test disponibles dans `bashunit`, telles que `assert_true`, `assert_false` ou `assert_command` pour tester la sortie d'une commande.

## Plongée profonde

Il existe également d'autres outils de test pour Bash, tels que `shunit2` ou `BATS`, qui offrent plus de fonctionnalités et de flexibilité. Cependant, l'utilitaire `bashunit` reste une option simple et efficace pour écrire des tests en Bash.

Il est important de noter que les tests ne doivent pas être considérés comme un remplacement des bonnes pratiques de codage. Il est toujours recommandé de suivre les normes de codage et de débogage pour éviter les erreurs.

# Voir aussi

- [bashunit sur GitHub](https://github.com/kward/shunit2)
- [shunit2 - Tutoriel de mise en route](https://www.toptal.com/software/testing/shunit2-shell-testing-utility-tutorial)
- [BATS - Site officiel](https://github.com/bats-core/bats-core)
- [Introduction aux tests d'intégration avec BATS](https://medium.com/@dallasbille/introduction-to-integration-testing-with-bats-af9e5ba537e1)