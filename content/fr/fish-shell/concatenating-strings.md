---
title:    "Fish Shell: Concaténer des chaînes de caractères."
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Dans le monde de la programmation, il est souvent nécessaire de manipuler des chaînes de caractères et de les combiner pour obtenir un résultat souhaité. Dans cet article, nous allons explorer comment utiliser la fonction de concaténation de la "Fish Shell" pour combiner efficacement des chaînes de caractères.

## Comment faire

La concaténation est le processus de combinaison de plusieurs chaînes de caractères pour en créer une seule. Dans la Fish Shell, cela peut être accompli facilement en utilisant l'opérateur de concaténation `+`.

Voici un exemple simple de concaténation de deux chaînes de caractères `Hello` et `World` :

```
Fish Shell:

set hello "Hello"
set world "World"
echo $hello + $world
```

Le résultat de cette opération sera `Hello World`. Comme vous pouvez le voir, l'opérateur `+` combine simplement les deux chaînes de caractères ensemble. 

Vous pouvez également concaténer des variables avec du texte en utilisant l'opérateur `+`. Par exemple, si nous voulons ajouter une salutation à notre chaîne `hello`, nous pouvons faire comme ceci :

```
Fish Shell:
set hello "Hello"
echo $hello + "there"
```

Le résultat sera `Hello there`.

## Plongée en profondeur

Il est important de noter que la fonction de concaténation en Fish Shell renvoie une nouvelle chaîne de caractères, elle ne modifie pas les chaînes de caractères d'origine.

De plus, si vous utilisez l'opérateur `+` pour concaténer des variables qui contiennent des espaces, assurez-vous d'entourer les variables de guillemets doubles pour éviter les erreurs.

Il est également possible de concaténer plusieurs chaînes de caractères en une seule ligne en utilisant l'opérateur `+` plusieurs fois.

## Voir aussi

- Documentation Fish Shell sur la concaténation : https://fishshell.com/docs/current/cmds/set.html
- Tutoriel sur la manipulation de chaînes de caractères en Fish Shell : https://hackmd.io/@fish-shell/how-to-manipulate-string-in-fish#Concat%C3%A9ner-des-strings-en-Fish-Shell