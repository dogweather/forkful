---
title:                "Mise en majuscule d'une chaîne de caractères"
html_title:           "PHP: Mise en majuscule d'une chaîne de caractères"
simple_title:         "Mise en majuscule d'une chaîne de caractères"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être familiarisé avec la fonction "ucfirst" en PHP, qui permet de mettre en majuscule la première lettre d'une chaîne de caractères. Mais saviez-vous qu'il existe également une fonction pour capitaliser entièrement une chaîne ? Dans cet article, nous allons voir pourquoi et comment utiliser la fonction "strtoupper" en PHP.

## Comment faire

Pour capitaliser une chaîne de caractères en utilisant la fonction "strtoupper", vous devez lui passer la chaîne en paramètre et stocker le résultat dans une variable. Voici un exemple de code :

```PHP
$chaine = "bonjour tout le monde";
$chaine_capitalisee = strtoupper($chaine);
echo $chaine_capitalisee; // affichera "BONJOUR TOUT LE MONDE"
```

Comme vous pouvez le voir, la fonction simplet "strtoupper" est très pratique pour capitaliser une chaîne de caractères en une seule ligne de code.

## Plongée en profondeur

La fonction "strtoupper" peut sembler assez simple, mais elle est en fait plus complexe qu'il n'y paraît. Tout d'abord, il est important de noter que cette fonction ne prend en compte que les caractères ASCII, c'est-à-dire les lettres de l'alphabet latin. Les caractères accentués ou spéciaux ne seront pas convertis en majuscules.

De plus, la fonction "strtoupper" ne tient pas compte de la casse initiale de la chaîne. Cela signifie qu'elle capitalisera toutes les lettres, sans distinction entre les minuscules et les majuscules. Par exemple, si vous passez la chaîne "Bonjour", la fonction la convertira en "BONJOUR" et non pas en "Bonjour".

Un autre aspect à prendre en compte est que la fonction "strtoupper" ne modifie pas la chaîne de caractères originale, mais retourne plutôt une nouvelle chaîne capitalisée. Cela peut être utile si vous voulez garder la version originale de la chaîne tout en capitalisant une copie.

Enfin, il faut également mentionner que la fonction "strtoupper" est sensible à l'environnement de codage de caractères dans lequel vous travaillez. Si votre chaîne contient des caractères non ASCII, vous pourriez obtenir des résultats inattendus.

## Voir aussi

- Documentation officielle de la fonction "strtoupper" en PHP : [strtoupper doc](https://www.php.net/manual/fr/function.strtoupper.php) 
- Tutoriel sur les fonctions de manipulation de chaînes en PHP : [Manipulation de chaînes en PHP](https://openclassrooms.com/fr/courses/918836-concevez-votre-site-web-avec-php-et-mysql/913477-les-chaines-de-caractere)
- Exemples pratiques d'utilisation de la fonction "strtoupper" : [10 petits exemples sur le PHP Strings](https://www.w3schools.com/php/php_string.asp)