---
title:    "Gleam: Écrire des tests"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Pourquoi écrire des tests en Gleam ?

Ecrire des tests est essentiel pour tout développeur, car cela garantit que votre code fonctionne correctement et cela vous évite de nombreuses heures de débogage. En écrivant des tests en Gleam, vous vous assurez que votre code est bien conçu et qu'il répond à toutes les exigences avant même de le déployer. 

## Comment faire pour écrire des tests en Gleam ?

Voici un exemple simple de test en Gleam, qui vérifie si une liste donnée contient un élément spécifique. 

```
Gleam
test "Vérifier si un élément existe dans une liste" {
    // Déclarer une liste avec des éléments 
    let liste = [1, 2, 3, 4, 5]
    // Déclarer l'élément à chercher 
    let element = 3
    // Vérifier si la liste contient l'élément 
    assert liste |> List.contains(element)
}
```

L'exemple ci-dessus montre comment utiliser la fonction `List.contains` pour vérifier si un élément donné est présent dans une liste. Vous pouvez également utiliser des blocs de code ```Gleam ... ``` pour écrire des tests plus complexes, comme vérifier si une fonction renvoie la valeur attendue. 

## Approfondissement 

En plus d'écrire des tests pour vérifier la précision de votre code, vous pouvez également utiliser des tests pour documenter le fonctionnement de votre code. Cela peut être utile pour les autres développeurs qui travaillent sur votre projet, car ils peuvent consulter les tests pour comprendre comment le code est censé fonctionner. De plus, en écrivant des tests, vous pouvez détecter rapidement les changements imprévus dans votre code et les corriger avant de les déployer. 

## Voir aussi 

- [Documentation sur les tests en Gleam](https://gleam.run/book/testing.html)
- [Exemples de tests en Gleam](https://github.com/gleam-lang/gleam/tree/master/examples/test)