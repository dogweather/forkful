---
title:                "Javascript: Affichage des sorties de débogage"
simple_title:         "Affichage des sorties de débogage"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur, vous savez sans doute à quel point le débogage peut être frustrant. Vous passez des heures à chercher l'erreur dans votre code, sans succès. C'est là qu'intervient l'impression de debug. En ajoutant des instructions d'impression dans votre code, vous pouvez facilement suivre le flux de votre programme et trouver l'endroit où l'erreur se produit. Cela peut vous faire gagner un temps considérable lors du débogage de votre code.

## Comment faire

Pour imprimer des instructions de débogage, vous pouvez utiliser la méthode `console.log()` en Javascript. Elle permet d'afficher des informations dans la console du navigateur ou du terminal. Par exemple, si vous souhaitez vérifier la valeur d'une variable, vous pouvez utiliser `console.log('Ma variable : ' + maVariable)` pour afficher la valeur de la variable.

```Javascript
const age = 26;
console.log('Mon âge : ' + age);
```

Cela affichera dans la console : `Mon âge : 26`. Vous pouvez également utiliser cette méthode pour afficher le résultat d'une opération ou le contenu d'un objet.

```Javascript
const fruits = ['pomme', 'banane', 'orange'];
console.log('Mes fruits préférés : ' + fruits);
```

Cela affichera dans la console : `Mes fruits préférés : pomme, banane, orange`.

## Plongée en profondeur

La méthode `console.log()` peut également être utilisée pour afficher des messages de débogage. Par exemple, si vous avez des conditions dans votre code et que vous n'êtes pas sûr de laquelle est exécutée, vous pouvez utiliser `console.log()` pour afficher un message à chaque condition.

```Javascript
const score = 10;

if (score < 5) {
  console.log('Score inférieur à 5');
} else if (score < 10) {
  console.log('Score compris entre 5 et 10');
} else {
  console.log('Score supérieur à 10');
}
```

Cela affichera dans la console : `Score compris entre 5 et 10`.

Il est important de noter que vous devriez supprimer les instructions d'impression de débogage une fois que vous avez résolu le problème, afin de garder votre code propre et efficace.

## Voir aussi

- [Documentation officielle de console.log() en Javascript](https://developer.mozilla.org/fr/docs/Web/API/Console/log)
- [Meilleures pratiques de débogage en Javascript](https://www.javascript.com/blog/javascript-debugging-tips-and-tricks)