---
title:    "TypeScript: Afficher les sorties de débogage"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Pourquoi

Le débogage est crucial lorsqu'il s'agit de résoudre les problèmes dans nos programmes. L'impression de messages de débogage est une excellente façon de suivre l'exécution de notre code et de localiser les erreurs plus rapidement.

## Comment faire

Pour imprimer du contenu de débogage en TypeScript, nous pouvons utiliser la méthode `console.log()`. Il s'agit d'une fonction intégrée qui prend en paramètre les données que nous voulons afficher. Voici un exemple de code:

```TypeScript
const nombre: number = 5;
const message: string = "Le nombre est de " + nombre;
console.log(message);
```

Lorsque nous exécutons ce code, nous obtenons le message suivant dans notre console:

```
Le nombre est de 5
```

Nous pouvons également utiliser cette méthode pour afficher des objets ou des tableaux en utilisant la syntaxe suivante :

```TypeScript
console.log("Voici un objet", { nom: "John", age: 25 });
console.log("Voici un tableau", [1, 2, 3]);
```

## Plongée en profondeur

Dans TypeScript, nous pouvons également utiliser la méthode `console.debug()` pour imprimer des informations de débogage. La différence avec `console.log()` est que les messages de débogage ne seront pas affichés par défaut dans la console, à moins que nous ne spécifions l'option de lancement `--inspect` lors de l'exécution de notre code.

Nous pouvons également améliorer la lisibilité de nos messages de débogage en utilisant des balises de couleur dans notre code. Par exemple :

```TypeScript
console.log("%c Voici un message important", "color: red");
```

Cela appliquera une couleur rouge au message "Voici un message important", ce qui peut être utile pour attirer l'attention sur certaines informations importantes.

## Voir aussi

- [Documentation officielle de TypeScript sur la méthode console.log()](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html#improved-logging)
- [Article sur l'utilisation de console.debug() en TypeScript](https://www.digitalocean.com/community/tutorials/how-to-use-console-debug-in-node-js)
- [Liste complète des balises de couleur pour le débogage en JavaScript](https://bgrins.github.io/devtools-snippets/#console-save)