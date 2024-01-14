---
title:                "Javascript: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi
Les arguments de ligne de commande sont un élément essentiel de la programmation en Javascript. Non seulement ils permettent une interaction directe avec le programme, mais ils facilitent également la personnalisation des valeurs des variables à chaque exécution.

## Comment faire
Pour lire les arguments de ligne de commande en Javascript, tout d'abord, vous devez utiliser l'objet process.argv. Cet objet contient un tableau avec tous les arguments passés lors de l'exécution du programme. Voici un exemple de code montrant comment utiliser les arguments de ligne de commande pour effectuer une addition :

```Javascript
// Récupération des arguments de ligne de commande
var args = process.argv;
// Conversion des arguments en nombres
var num1 = parseFloat(args[2]);
var num2 = parseFloat(args[3]);
// Addition des nombres
var result = num1 + num2;
// Affichage du résultat
console.log("Le résultat de l'addition est : " + result);
```

Si vous exécutez ce programme avec les arguments `4` et `6`, le résultat sera `Le résultat de l'addition est : 10`. Vous pouvez également passer plusieurs arguments en les séparant par des espaces.

## Plongée profonde
Outre la récupération et la manipulation des valeurs des arguments, il est également possible de les utiliser pour effectuer des actions spécifiques en fonction de leur contenu. Par exemple, si vous souhaitez masquer un mot de passe saisi en ligne de commande, vous pouvez utiliser un argument spécifique et afficher des astérisques à la place des caractères :

```Javascript
// Récupération de l'argument "password" et affichage des astérisques
var password = process.argv[2];
var hiddenPassword = password.replace(/./g, "*");
console.log("Votre mot de passe est : " + hiddenPassword);
```

Si vous exécutez ce programme avec l'argument `password`, le résultat sera `Votre mot de passe est : ********`.

## Voir aussi
Si vous souhaitez en savoir plus sur les arguments de ligne de commande en Javascript, voici quelques liens utiles :

- [Documentation officielle de Node.js sur process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Blog post sur la récupération des arguments en Javascript](https://scotch.io/tutorials/nodejs-command-line-arguments-node-js-command-line-tutorial) (en anglais)
- [Exemple de projet utilisant des arguments de ligne de commande en Javascript](https://github.com/RobertWHurst/node-cli-boilerplate) (en anglais)