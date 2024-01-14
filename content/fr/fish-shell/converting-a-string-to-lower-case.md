---
title:    "Fish Shell: Transformant une chaîne en minuscules"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être en train de programmer en Fish Shell et vous vous demandez pourquoi convertir une chaîne de caractères en minuscules est important. Eh bien, cela peut être utile dans certaines situations, comme lors de la comparaison de chaînes de caractères qui peuvent contenir des majuscules ou des minuscules, ou lorsqu'une API demande des données en minuscules.

Maintenant que nous avons établi l'importance de cette tâche, voyons comment la réaliser en utilisant Fish Shell.

## Comment faire

Pour convertir une chaîne de caractères en minuscules en utilisant Fish Shell, nous pouvons utiliser la fonction intégrée `string tolower`. Elle prendra en argument la chaîne de caractères que nous voulons convertir en minuscules.

```
Fish Shell
set text "Bonjour le Monde"
echo (string tolower $text)
```

Cela produira la sortie suivante :

```
bonjour le monde
```

Nous pouvons également utiliser la substitution de commandes pour convertir une chaîne de caractères en minuscules. Cela fonctionne en utilisant la commande `tr` pour transformer les lettres en minuscules.

```
Fish Shell
set text "Bonjour le Monde"
echo (tr '[A-Z]' '[a-z]' <<< $text)
```

La sortie sera la même que dans l'exemple précédent :

```
bonjour le monde
```

## Plongée en profondeur

Maintenant que nous savons comment convertir une chaîne de caractères en minuscules en utilisant Fish Shell, examinons quelques détails supplémentaires à propos de cette tâche. Tout d'abord, il est important de noter que la fonction `string tolower` ne modifie pas la chaîne de caractères d'origine, elle retourne plutôt une nouvelle chaîne de caractères en minuscules. Cela peut être utile si vous voulez stocker à la fois la chaîne d'origine et la version en minuscules.

En outre, il est également possible d'utiliser la substitution de commandes pour convertir une chaîne de caractères en minuscules sans avoir à utiliser la commande `tr`. Cela peut être fait en utilisant un paramètre spécial `$home` pour accéder au répertoire de l'utilisateur et en utilisant `sed` pour transformer les lettres en minuscules.

```
Fish Shell
set text "Bonjour le Monde"
echo (sed 's/.*/\L&/' <<< $text)
```

La sortie sera toujours la même :

```
bonjour le monde
```

## Voir aussi

Maintenant que vous savez comment convertir une chaîne de caractères en minuscules en utilisant Fish Shell, vous pouvez l'utiliser pour améliorer vos scripts ou vos programmes. Pour plus d'informations sur les fonctions intégrées à Fish Shell, consultez la documentation officielle sur leur site web.

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/functions.html)
- [Guide de démarrage rapide de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Tutoriel vidéo pour débutants sur Fish Shell](https://www.youtube.com/watch?v=c3PYvoVdqqc)