---
title:                "Lecture des arguments en ligne de commande"
html_title:           "Javascript: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur, vous avez sûrement déjà entendu parler de l'utilisation des arguments de ligne de commande en programmation Javascript. Mais pourquoi devriez-vous vous intéresser à cette fonctionnalité ? Eh bien, tout simplement parce que cela peut vous faire gagner un temps précieux et rendre votre code plus flexible et puissant.

## Comment Faire

Pour lire les arguments de ligne de commande dans votre code Javascript, vous pouvez utiliser l'objet `process` et sa méthode `argv`. Le tableau `argv` contient une liste des arguments passés en ligne de commande, avec le premier élément étant le chemin vers le fichier Javascript lui-même.

Voici un exemple de code qui affiche tous les arguments passés en ligne de commande :

```javascript
var arguments = process.argv;
for (var i = 0; i < arguments.length; i++) {
  console.log("Argument " + i + ": " + arguments[i]);
}
```

Si vous exécutez ce code avec les arguments "node index.js hello world", vous obtiendrez la sortie suivante :

```bash
Argument 0: /usr/local/bin/node
Argument 1: /chemin/vers/votre/fichier/javascript
Argument 2: hello
Argument 3: world
```

Vous pouvez également accéder à un argument spécifique en utilisant son index dans le tableau `argv`. Par exemple, pour récupérer seulement le premier argument, vous pouvez utiliser `process.argv[2]`.

## Approfondissement

Maintenant que vous savez comment lire les arguments de ligne de commande, il est important de comprendre comment les utiliser correctement. Voici quelques bonnes pratiques à garder en tête :

- Si vous devez passer des valeurs numériques en ligne de commande, pensez à les convertir en nombre en utilisant `parseInt` ou `parseFloat` pour éviter les problèmes de type.
- Vous pouvez également utiliser des options de ligne de commande en utilisant un module comme `yargs` pour rendre votre code plus lisible et facile à comprendre.
- N'oubliez pas que les arguments de ligne de commande sont sensibles à la casse, donc "hello" et "Hello" seront considérés comme deux arguments différents.

## Voir Aussi

Pour en savoir plus sur les arguments de ligne de commande en Javascript, vous pouvez consulter les liens suivants :

- [Documentation sur les arguments de ligne de commande dans Node.js](https://nodejs.org/api/process.html#process_process_argv)
- [Tutoriel sur la gestion des arguments de ligne de commande avec yargs](https://www.sitepoint.com/command-line-utilities-with-node-js/)
- [Article sur l'utilisation des arguments de ligne de commande en Javascript pour les débutants](https://www.abeautifulsite.net/accessing-command-line-arguments-in-node-js)