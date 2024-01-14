---
title:    "Javascript: Lecture des arguments de ligne de commande"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi

La lecture des arguments de ligne de commande est une compétence importante pour tout développeur en Javascript. Que vous soyez un débutant ou un expert, comprendre comment utiliser les arguments de ligne de commande peut vous aider à améliorer vos compétences en programmation et à rendre vos scripts plus dynamiques et polyvalents.

## Comment Faire

Pour lire les arguments de ligne de commande en Javascript, vous pouvez utiliser l'objet process.argv. Cet objet contient un tableau de tous les arguments passés à votre programme, y compris le nom de fichier du script lui-même. Voici un exemple de code:

```Javascript
//Récupération du deuxième argument passé en ligne de commande
let arg = process.argv[2];
//Affichage du résultat dans la console
console.log("L'argument passé est: " + arg);
```

Si vous exécutez le script avec `node script.js argument`, vous devriez voir la sortie suivante: `L'argument passé est: argument`.

Vous pouvez également utiliser les arguments pour effectuer différentes actions en fonction de la valeur passée. Par exemple, vous pouvez utiliser une structure conditionnelle pour déclencher certaines fonctions en fonction de l'argument passé. Voici un exemple:

```Javascript
//Récupération du deuxième argument passé en ligne de commande
let arg = process.argv[2];
//Vérification de la valeur de l'argument
if (arg === "print") {
  console.log("Le script a été lancé avec l'argument 'print'");
} else if (arg === "save") {
  console.log("Le script a été lancé avec l'argument 'save'");
} else {
  console.log("L'argument n'est pas pris en charge");
}
```

Si vous exécutez le script avec `node script.js print`, vous devriez voir la sortie suivante: `Le script a été lancé avec l'argument 'print'`.

## Plongée Profonde

Si vous avez besoin d'une compréhension plus approfondie des arguments de ligne de commande, vous pouvez également consulter la documentation officielle de Node.js sur l'objet process.argv. Vous y trouverez plus d'informations sur les différentes méthodes et propriétés de cet objet, ainsi que des exemples plus avancés.

Vous pouvez également consulter des bibliothèques comme yargs qui facilitent la gestion des arguments de ligne de commande dans vos scripts. Ces bibliothèques offrent des fonctionnalités supplémentaires telles que la validation des arguments et la création de commandes et d'options de manière plus structurée.

## Voir Aussi

- [Documentation officielle de Node.js sur l'objet process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [Bibliothèque yargs pour la gestion des arguments de ligne de commande en Javascript](http://yargs.js.org/)