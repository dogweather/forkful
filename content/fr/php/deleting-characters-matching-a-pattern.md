---
title:    "PHP: Suppression de caractères correspondant à un motif"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Pourquoi supprimer des caractères correspondants à un modèle

Vous êtes peut-être confronté à une situation où vous avez besoin de supprimer certains caractères d'une chaîne de texte en PHP. Que ce soit pour nettoyer des données ou pour remplacer une partie de la chaîne par une autre, la suppression de caractères correspondants à un modèle peut être une tâche utile en programmation.

## Comment faire

En PHP, il existe plusieurs façons de supprimer des caractères correspondants à un modèle. L'une des options est d'utiliser la fonction `preg_replace()` qui prend en paramètre une expression régulière pour spécifier le modèle à supprimer. Voici un exemple de code utilisant cette fonction :

```PHP
// Chaine de texte originale
$texte = 'Bonjour, PHP rocks!';

// Supprimer les lettres en majuscule
$texte = preg_replace('/[A-Z]/', '', $texte);

// Résultat : 'onjour, h rocks!'
```

Dans cet exemple, l'expression régulière `/[A-Z]/` spécifie de supprimer toutes les lettres en majuscule. La fonction `preg_replace()` renvoie alors le texte modifié avec ces lettres supprimées.

Il existe également d'autres fonctions utiles comme `str_replace()` qui remplace toutes les occurrences d'une chaîne par une autre, et `trim()` qui supprime les espaces en début et en fin de chaîne.

## Plongée en profondeur

En utilisant l'expression régulière, vous pouvez avoir un contrôle plus précis sur les caractères à supprimer. Par exemple, si vous avez besoin de supprimer tous les chiffres de votre chaîne de texte, vous pouvez utiliser l'expression régulière `/\d/` qui spécifie de supprimer tous les chiffres.

De plus, vous pouvez utiliser des modificateurs avec votre expression régulière pour affiner votre motif de suppression. Par exemple, le modificateur `i` peut être ajouté pour ignorer la casse, ou `g` pour supprimer toutes les occurrences d'un caractère.

## Voir aussi

Si vous souhaitez approfondir vos connaissances sur les expressions régulières et leurs différentes applications, voici une liste de liens utiles :

- Documentation officielle de PHP sur les expressions régulières : https://www.php.net/manual/fr/reference.pcre.pattern.syntax.php
- Un tutoriel complet sur les expressions régulières en PHP : https://openclassrooms.com/fr/courses/918836-concevez-vos-sites-web-avec-php-et-mysql/913922-tp-les-expressions-regulieres-en-php
- Un outil en ligne pour tester vos expressions régulières : https://regex101.com/

Maintenant que vous connaissez différentes façons de supprimer des caractères correspondants à un modèle en PHP, vous pouvez les utiliser dans vos projets pour gagner du temps et simplifier votre code. N'hésitez pas à explorer d'autres fonctions et modificateurs pour trouver celui qui convient le mieux à votre cas d'utilisation !