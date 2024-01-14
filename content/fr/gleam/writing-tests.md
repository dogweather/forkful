---
title:                "Gleam: Écriture de tests"
simple_title:         "Écriture de tests"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi écrire des tests dans Gleam ?

Les tests jouent un rôle essentiel dans le développement de logiciels de qualité. Ils nous permettent de vérifier que notre code fonctionne comme prévu et de détecter les erreurs rapidement. Dans cet article, nous allons voir pourquoi il est important d'écrire des tests dans Gleam et comment le faire efficacement.

## Comment écrire des tests dans Gleam ?

Ecrire des tests dans Gleam est très simple grâce à son approche de programmation fonctionnelle et à son support intégré pour les tests. Prenons par exemple une fonction qui calcule le carré d'un nombre :

```Gleam
fn carre(x) {
  x * x
}
```

Pour écrire un test pour cette fonction, nous allons utiliser le module "assert" fourni par Gleam. Nous pouvons définir notre test comme ceci :

```Gleam
test "Teste le calcul du carré" {
  assert.equal(carre(4), 16)
  assert.equal(carre(10), 100)
}
```

Maintenant, si nous exécutons ces tests dans notre terminal avec la commande `gleam test`, nous devrions voir une sortie comme ceci :

```
✓ Teste le calcul du carré
```

Nous obtenons le symbole ✓ pour indiquer que notre test a réussi. Si jamais notre fonction venait à être modifiée et que le test ne passe plus, nous serions immédiatement avertis et pourrions corriger notre fonction en conséquence.

Gleam vous permet également d'organiser vos tests en "modules" pour une meilleure organisation et une exécution plus sélective. Vous pouvez en apprendre plus à ce sujet dans la documentation officielle de Gleam.

## Plongée plus profonde sur l'écriture de tests

Il est important de noter que les tests ne doivent pas seulement être écrits pour vérifier le bon fonctionnement de notre code, mais aussi pour couvrir tous les cas possibles. Cela signifie qu'il est important de tester les cas de succès, mais aussi les cas d'échec et les cas limites. Plus vos tests sont complets, moins vous aurez de bugs à gérer dans votre code.

De plus, l'écriture de tests vous mène souvent à un développement plus modulaire. En découpant votre code en fonctions plus petites et plus ciblées, il devient plus facile de les tester individuellement et de les combiner pour des fonctionnalités plus complexes. Cela mène également à un code plus maintenable et extensible.

N'hésitez pas à consulter la documentation officielle de Gleam pour des conseils sur la façon d'écrire de bons tests et comment intégrer les tests dans votre workflow de développement.

## Voir aussi

- [Documentation officielle de Gleam](https://gleam.run/documentation/)
- [Guide complet pour écrire des tests en Gleam](https://gleam.run/documentation/testing)
- [Exemples de tests en Gleam](https://github.com/gleam-lang/gleam/blob/master/examples/tests)