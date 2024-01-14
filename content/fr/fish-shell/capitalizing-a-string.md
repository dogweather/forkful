---
title:                "Fish Shell: Mise en majuscule d'une chaîne de caractères"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

La programmation en Fish Shell peut sembler intimidante au début, mais elle offre de nombreuses fonctionnalités utiles pour les utilisateurs avancés. L'une de ces fonctionnalités est la possibilité de mettre en majuscule une chaîne de caractères. Pourquoi voudriez-vous le faire ? Peut-être que vous travaillez avec des données qui nécessitent une mise en forme spécifique, ou peut-être que vous voulez simplement ajouter un peu de style à vos résultats de script. Quelle que soit votre raison, apprendre à mettre en majuscule une chaîne de caractères en Fish Shell peut vous être très utile.

## Comment faire

Pour mettre une chaîne de caractères en majuscule en utilisant Fish Shell, voici un exemple de code à suivre :

```
set string "ceci est un exemple"
echo $string | tr a-z A-Z
```

Dans cet exemple, nous avons créé une variable "string" contenant la chaîne de caractères "ceci est un exemple". En utilisant la commande "tr" avec les options "a-z" et "A-Z", nous convertissons toutes les lettres minuscules en lettres majuscules. En exécutant ce code, nous obtenons le résultat suivant :

```
CECI EST UN EXEMPLE
```

Il est également possible d'utiliser la fonction "string" pour mettre en majuscule une chaîne de caractères. Par exemple :

```
set string "ceci est un exemple"
echo $string | string upper
```

Cette fois-ci, nous utilisons la fonction "upper" de la commande "string" pour convertir la chaîne de caractères en majuscules. Cela donne le même résultat que dans l'exemple précédent.

## Plongée en profondeur

Maintenant que vous savez comment mettre en majuscule une chaîne de caractères en Fish Shell, passons à un peu plus de détails. Il est important de noter que la conversion en majuscules ne fonctionne que pour les caractères ASCII. Cela signifie que les caractères accentués ou spéciaux ne seront pas convertis en majuscules.

De plus, vous pouvez également utiliser la variable interne "$USER" pour mettre en majuscule le nom d'utilisateur de la session en cours. Par exemple :

```
echo $USER | tr a-z A-Z
```

Et si vous préférez utiliser la fonction "string", vous pouvez également le faire comme ceci :

```
echo $USER | string upper
```

## Voir aussi

Pour en savoir plus sur la programmation en Fish Shell, vous pouvez consulter les liens suivants :

- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [Fish Shell Command Substitution](https://fishshell.com/docs/current/index.html#command-substitution)

Maintenant que vous avez appris à mettre en majuscule une chaîne de caractères en Fish Shell, vous pouvez l'appliquer à différents cas d'utilisation dans vos scripts et vos tâches quotidiennes. Amusez-vous bien !