---
title:                "Écriture de tests"
html_title:           "PHP: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire des tests est souvent considéré comme une tâche fastidieuse et inutile par de nombreux développeurs. Cependant, c'est une pratique essentielle pour garantir la qualité et la fiabilité de votre code. Les tests vous permettent de détecter et de corriger rapidement les bugs, de faciliter les développements futurs et de maintenir un code cohérent et évolutif.

## Comment Faire

Pour commencer à écrire des tests en PHP, vous aurez besoin d'un framework de test tel que PHPUnit et d'un environnement de développement configuré pour les tests.

Vous pouvez utiliser l'annotation `@test` pour marquer une méthode comme un test à exécuter. Ensuite, utilisez les assertions pour vérifier si le résultat de votre code correspond à celui attendu. Voici un exemple de test simple pour une fonction de multiplication :

```PHP
/**
 * @test
 */
public function testMultiplication()
{
  $result = multiply(2, 3);
  $this->assertEquals(6, $result);
}
```

Une fois que vous avez écrit vos tests, exécutez-les en utilisant la commande `phpunit` dans votre terminal. Vous devriez voir les résultats des tests ainsi que les éventuels échecs et erreurs.

## Deep Dive

Les tests sont généralement divisés en trois catégories : les tests unitaires, les tests fonctionnels et les tests d'intégration. Les tests unitaires visent à tester une seule unité de code, tandis que les tests fonctionnels vérifient le comportement d'une fonctionnalité complète de l'application. Les tests d'intégration, quant à eux, vérifient que toutes les parties de l'application fonctionnent correctement ensemble.

Il est important de noter que les tests ne doivent pas seulement être exécutés une fois lors de la création d'une fonctionnalité, mais doivent également être régulièrement exécutés lors de modifications ou de mises à jour du code. Cela garantit que toutes les parties de votre application restent cohérentes et fonctionnelles malgré les changements.

Enfin, il est préférable de suivre une approche de développement pilotée par les tests (TDD) où les tests sont écrits avant le code afin de garantir une meilleure couverture des tests et une meilleure qualité de code.

## See Also

- [PHPUnit](https://phpunit.de/)
- [Approche TDD en PHP](https://code.tutsplus.com/fr/tutorials/test-driven-development-in-php--net-25796)
- [Tests en PHP : comprendre les types de tests](https://www.supinfo.com/articles/single/3994-tests-php-comprendre-types-tests)