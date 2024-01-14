---
title:    "Elm: Écrire des tests"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Pourquoi il est important d'écrire des tests en Elm

Lorsqu'on programme en Elm, il est essentiel d'écrire des tests pour garantir un code fiable et de qualité. Les tests permettent de s'assurer que le code produit fonctionne correctement et de détecter les éventuelles erreurs avant qu'elles ne causent des problèmes dans l'application.

## Comment écrire des tests en Elm

Ecrire des tests en Elm est relativement simple grâce à la bibliothèque de tests intégrée dans le langage appelée `elm-test`. Voici un exemple de test unitaire pour une fonction `multiply`:

```
elm-test
  import Multiplication exposing (multiply)

multiplyTest : Test
multiplyTest =
  test "La multiplication de 3 par 2 donne bien 6" <|
    \() ->
      Expect.equal (multiply 3 2) 6

```

En exécutant la commande `elm-test` dans votre terminal, vous obtiendrez le résultat suivant:

```
Success! Passed 1 test.
```

Il est également possible d'écrire des tests plus complexes avec des fonctions telles que `Expect.notEqual`, `Expect.isTrue`, `Expect.fail`, etc. La documentation complète de `elm-test` est disponible sur leur site web.

## Plongée plus profonde dans l'écriture de tests en Elm

Ecrire des tests en Elm ne consiste pas seulement à vérifier si un code produit le résultat attendu. Les tests permettent également d'encourager une bonne pratique de développement en favorisant la modularité et la réutilisabilité du code. En effet, pour pouvoir être testée, une fonction doit être indépendante et avoir des entrées définies et prévisibles.

De plus, les tests se révèlent très utiles lors de la maintenance du code. Si vous devez effectuer des modifications ou des ajouts, les tests permettent de rapidement détecter si ces changements ont des conséquences négatives sur le reste de l'application.

# Voir aussi

- Site officiel de Elm: https://elm-lang.org/
- Documentation sur les tests en Elm: https://package.elm-lang.org/packages/elm-explorations/test/latest/
- Article sur les bonnes pratiques de tests en Elm: https://thoughtbot.com/blog/testing-in-elm