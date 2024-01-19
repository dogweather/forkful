---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
La concaténation de chaînes, c'est l'assemblage de plusieurs chaînes de caractères en une seule. Les programmeurs le font pour manipuler ou modifier des données textuelles.

## Comment Concaténer les Chaînes en Fish 
En Fish Shell, la concaténation des chaînes est simple et directe. Voici un exemple :

```fish
set str1 "Bonjour"
set str2 ", monde!"
echo $str1$str2
```

La sortie sera :

```
Bonjour, monde!
```

## Entrons dans les Détails
Historiquement, la concaténation de chaînes a des racines profondes dans la programmation informatique - elle existait bien avant l'avènement des langages modernes comme Fish. C'est un concept universel dans tous les langages de programmation.

Il existe des alternatives à la concaténation directe de chaînes en Fish, comme l’utilisation de la fonction `string` :

```fish
set str1 "Bonjour"
set str2 ", monde!"
echo (string join "" $str1 $str2)
```

Le résultat serait le même qu’avant :

```
Bonjour, monde!
```

La concaténation de chaînes en Fish est réalisée en interne via le moteur Fish qui prend en charge les manipulations de chaînes. C'est une opération très rapide et généralement sans coût significatif en termes de performances.

## Pour En Savoir Plus
Pour en savoir plus sur la concaténation de chaînes et d'autres opérations de chaînes en Fish, consultez la documentation officielle de Fish Shell : [Fish Shell Documentation](https://fishshell.com/docs/current/index.html).