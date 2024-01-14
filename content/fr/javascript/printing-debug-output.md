---
title:    "Javascript: Afficher la sortie de débogage"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi 

Il peut être tentant de simplement supprimer les lignes de code qui déclenchent des erreurs ou des bugs dans votre programme, mais l'ajout d'instructions de débogage peut en fait être une étape très utile lors de la programmation en Javascript. En imprimant des informations sur l'exécution de votre code, vous pouvez comprendre ce qui se passe réellement et identifier plus facilement la source du problème. Cela peut vous faire gagner du temps et éliminer les frustrations liées à la recherche de bugs.

## Comment faire 

Pour ajouter des instructions de débogage à votre code Javascript, vous pouvez utiliser la méthode ``console.log()``. Cela imprimera tout ce que vous spécifiez entre les parenthèses dans la console de votre navigateur. Par exemple :

```Javascript
let num1 = 5;
let num2 = 10;
console.log("La somme est : " + (num1 + num2));
```

Cela imprimera dans la console : ``La somme est : 15``. Vous pouvez également utiliser ``console.error()`` pour afficher des messages d'erreur spécifiques en cas de besoin.

## Plongée en profondeur 

L'utilisation de méthodes de débogage comme ``console.log()`` peut également être utile lors de l'identification et de la correction de problèmes de performance. En imprimant des informations sur le temps d'exécution des différentes parties de votre code, vous pouvez identifier les parties qui prennent le plus de temps et les optimiser.

Il est également possible de formater les messages de débogage en utilisant des expressions régulières et des variables. Cela peut vous permettre de créer des messages plus clairs et utiles pour comprendre l'exécution de votre code.

## Voir aussi 

- [Guide sur l'utilisation des instructions de débogage en Javascript](https://developer.mozilla.org/fr/docs/Web/API/Console)
- [Tutoriel sur l'utilisation du débogueur Javascript intégré](https://www.digitalocean.com/community/tutorials/how-to-debug-javascript-introduction-with-example-code)
- [Article sur les meilleures pratiques pour le débogage en Javascript](https://blog.sessionstack.com/how-to-debug-javascript-like-a-pro-a2a27a5e04cc)