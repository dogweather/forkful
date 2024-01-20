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

## Qu'est-ce que c'est et pourquoi devriez-vous écrire des tests?

Ecrire des tests est une pratique courante parmi les programmeurs pour s'assurer que leur code fonctionne comme prévu. Cela consiste à écrire du code qui vérifie le bon fonctionnement de chaque partie du code écrit.

Les programmeurs écrivent des tests pour s'assurer que leur code est exempt de bugs et pour améliorer la qualité et la stabilité de leur application. Cela garantit également que de nouvelles modifications ou mises à jour n'ont pas d'impact négatif sur le code existant.

## Comment le faire:

```PHP
Function addition($a, $b) {
  return $a + $b;
}

echo addition(2, 3); // affiche 5
echo addition(-2, 5); // affiche 3
```

Ce code est un exemple de test pour une fonction qui effectue une simple addition. Nous appelons la fonction avec différents paramètres et vérifions si la valeur de retour est correcte. Si elle correspond à ce que nous attendions, cela signifie que notre fonction fonctionne correctement.

## Approfondissement:

La pratique d'écrire des tests a pris de l'importance ces dernières années avec l'essor des méthodes agiles de développement de logiciels. Elle est également étroitement liée à l'utilisation de la méthodologie de développement piloté par les tests (Test-Driven Development, ou TDD).

Il existe plusieurs outils pour écrire des tests en PHP, tels que PHPUnit et Codeception. Ces outils offrent une variété de fonctionnalités telles que l'exécution de tests automatisés, la couverture de code et la génération de rapports.

Il existe également des alternatives à l'écriture de tests en PHP, comme l'utilisation de langages de test dédiés tels que Cucumber ou Selenium. Ces langages sont largement utilisés pour des tests d'intégration ou de bout en bout.

## A voir également:

- [Documentation PHPUnit](https://phpunit.de/documentation.html)
- [Cucumber](https://cucumber.io/)
- [Selenium](https://www.selenium.dev/)