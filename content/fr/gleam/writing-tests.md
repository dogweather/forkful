---
title:    "Gleam: Écriture de tests"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-tests.md"
---

{{< edit_this_page >}}

# Pourquoi écrire des tests en Gleam ?

Écrire des tests est une pratique essentielle pour garantir la qualité de votre code en langue Gleam. Cela permet de détecter et de corriger les erreurs avant qu'elles ne deviennent un problème pour votre application. En outre, cela vous permettra de gagner du temps et de l'énergie à long terme en évitant des bogues coûteux.

## Comment faire

Il existe deux types de tests que vous pouvez écrire en Gleam : les tests unitaires et les tests d'intégration. Les tests unitaires se concentrent sur le test d'une seule fonction ou module, tandis que les tests d'intégration vérifient le fonctionnement de l'ensemble de votre application.

Pour écrire un test unitaire, vous pouvez utiliser la fonction `assert` en passant en paramètre une expression à évaluer et une valeur attendue. Par exemple :

```Gleam
test "vérifie si 2 + 2 est égal à 4" {
  assert 2 + 2 == 4   // cette expression est évaluée et comparée à 4
}
```

Si la valeur attendue n'est pas égale à l'expression évaluée, une erreur sera générée et votre test échouera. Vous pouvez également utiliser la fonction `assert_ok` pour tester le succès d'une fonction qui retourne un type `Result`. Par exemple :

```Gleam
test "vérifie si la fonction division retourne le résultat attendu" {
  assert_ok division(10, 2) == 5   // la fonction division doit retourner 5 pour ces paramètres
}
```

Pour écrire un test d'intégration, vous pouvez utiliser la fonction `scenario` qui simule l'utilisation de différentes parties de votre application. Par exemple :

```Gleam
test "vérifie si la fonction de connexion d'un utilisateur fonctionne correctement" {
  scenario "connexion avec un nom d'utilisateur et un mot de passe valides" {
    let username = "John"
    let password = "abc123"
    assert_ok login(username, password)
  }

  scenario "tentative de connexion avec des informations invalides" {
    let username = "Jane"
    let password = "wrongpass"
    assert_error login(username, password)
  }
}
```

Dans cet exemple, nous testons à la fois le succès et l'échec de la fonction de connexion en utilisant différentes combinaisons de noms d'utilisateur et mots de passe.

## Plongée en profondeur

Les tests peuvent également utiliser des modules externes pour simuler diverses conditions et mieux couvrir les différents scénarios possibles. Vous pouvez également écrire des tests pour vérifier que les messages d'erreur ou de réussite renvoyés par vos fonctions sont corrects.

Il est important de noter que les tests ne peuvent pas couvrir tous les cas possibles, mais ils peuvent grandement améliorer la qualité de votre code et vous assurer une plus grande confiance en celui-ci.

## Voir aussi

- [Documentation sur les tests en Gleam](https://gleam.run/book/intro)
- [Guide de style de code Gleam](https://gleam.run/book/change-the-name)
- [Utiliser des tests pour améliorer la qualité de votre code](https://www.freecodecamp.org/news/using-tests-to-improve-code-quality/)