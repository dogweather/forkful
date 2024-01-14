---
title:    "Fish Shell: Extraction de sous-chaînes"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est souvent nécessaire de travailler avec des chaînes de caractères et d'extraire des sous-chaînes de ces dernières. Que ce soit pour obtenir des informations spécifiques ou pour manipuler des données, l'extraction de sous-chaînes est une compétence utile à maîtriser. Dans cet article, nous allons vous montrer comment le faire en utilisant le langage de shell Fish.

## Comment faire

L'utilisation de la fonction de sous-chaîne dans Fish Shell est simple et efficace. Voici un exemple de code pour extraire une sous-chaîne à partir d'une chaîne de caractères :

```
set str "Bonjour tout le monde !"
echo $str[2..5]
```

Dans cet exemple, nous avons défini la variable `str` avec une chaîne de caractères à laquelle nous voulons extraire une sous-chaîne. En utilisant la syntaxe `$str[start..end]`, nous spécifions l'index de départ et de fin pour notre sous-chaîne. Dans cet exemple, nous récupérons les caractères de l'index 2 à 5 de la chaîne de caractères, ce qui nous donne "njour" comme résultat.

Nous pouvons également utiliser cette syntaxe pour extraire une sous-chaîne à partir de la fin de la chaîne de caractères en utilisant des index négatifs. Par exemple, `$str[-3..-1]` donnerait "de !" comme résultat.

L'utilisation de `..` signifie que nous incluons l'index de fin dans notre sous-chaîne. Si nous voulons exclure cet index, nous pouvons utiliser `...` à la place. Par exemple, `$str[2...5]` donnerait "njou" comme résultat.

Il est également possible d'utiliser des formules mathématiques pour définir les indices de début et de fin. Par exemple, `$str[2 + 3 * 2 .. -1]` donnerait "ur tout le monde !" comme résultat.

Grâce à ces différentes possibilités, nous pouvons facilement extraire des sous-chaînes précises avec Fish Shell.

## Plongée en profondeur

Il y a quelques points importants à garder à l'esprit lors de l'extraction de substrings avec Fish Shell. Tout d'abord, lorsque vous utilisez la syntaxe `..` et `...`, assurez-vous que votre index de début est inférieur à l'index de fin. Dans le cas contraire, vous obtiendrez un résultat vide.

De plus, si vous utilisez une formule mathématique pour définir les indices, assurez-vous de bien vérifier les calculs pour éviter les erreurs.

Il est également important de noter que la fonctionnalité de sous-chaîne dans Fish Shell ne fonctionne que pour les chaînes de caractères. Si vous essayez d'extraire une sous-chaîne à partir d'autres types de données, vous obtiendrez une erreur.

Enfin, la fonction de sous-chaîne ne modifie pas la chaîne de caractères originale, elle crée plutôt une nouvelle sous-chaîne à partir de celle-ci.

## Voir aussi

- [La documentation officielle de Fish Shell sur les sous-chaînes](https://fishshell.com/docs/current/cmds/substr.html)
- [Un guide complet sur l'utilisation de Fish Shell](https://dev.to/scottydocs/an-introduction-to-fish-shell-3m5j)
- [Exploration plus avancée des fonctionnalités de Fish Shell](https://bfgn.me/fish-shell.html)