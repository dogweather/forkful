---
title:                "Elm: Écrire des tests"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture de tests est un aspect important de la programmation en Elm. Les tests permettent de vérifier que votre code fonctionne correctement et de détecter d'éventuelles erreurs ou bugs avant qu'ils n'affectent votre application en production. Cela peut vous faire économiser beaucoup de temps et d'efforts dans la résolution de problèmes à l'avenir.

## Comment faire

Pour écrire des tests en Elm, vous aurez besoin du module "Elm Test". Vous pouvez l'importer dans votre code avec la ligne suivante :

```Elm
import Test exposing (..)
```

Ensuite, vous pouvez commencer à écrire vos tests en utilisant les fonctions "test" et "expect" fournies par le module. Voici un exemple simple de test d'égalité :

```Elm
myTest =
    test "Test d'égalité"
        (expect 2 1)
```

Dans cet exemple, nous utilisons la fonction "test" pour définir le nom de notre test, puis nous utilisons la fonction "expect" pour comparer les valeurs. Si les valeurs sont égales, le test passe. Sinon, il échoue.

Vous pouvez également utiliser des "flags" pour tester des fonctions plus complexes. Voici un exemple où nous testons la fonction "add" qui prend deux entiers en entrée et renvoie leur somme :

```Elm
addTest =
    test "Test d'addition"
        [ ( expect 5 (add 2 3) )
        , ( expect 0 (add -2 2) )
        ]
```

Dans ce cas, nous testons deux scénarios différents. Si les valeurs renvoyées par la fonction "add" correspondent à celles que nous attendons, le test passe.

## Plongée en profondeur

Il existe différentes stratégies pour écrire des tests en Elm, notamment les tests unitaires, les tests fonctionnels et les tests d'intégration. Les tests unitaires sont souvent utilisés pour tester des fonctions individuelles ou des modules, tandis que les tests fonctionnels peuvent tester des scénarios d'utilisation réelle de votre application. Les tests d'intégration peuvent être utilisés pour tester l'interaction entre différents composants de votre application.

Il est également important de noter que les tests ne doivent pas être considérés comme une étape distincte de votre processus de développement. Idéalement, les tests devraient être écrits parallèlement à votre code afin de détecter rapidement les bugs et de garantir un code propre et fonctionnel.

## Voir aussi

- Guide officiel pour les tests en Elm : https://guide.elm-lang.org/testing/
- Vidéo explicative sur les tests en Elm : https://www.youtube.com/watch?v=ZB8Sa2lAsM4
- Article sur la philosophie des tests en Elm : https://medium.com/@wking__/testing-in-elm-part-1-philosophy-98f3e4fff747