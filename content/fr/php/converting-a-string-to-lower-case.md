---
title:    "PHP: Convertir une chaîne en minuscules"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Pourquoi

La conversion de chaînes de caractères en minuscules est une tâche courante lors de la programmation en PHP. Cela peut être utile pour comparer et trier des données ou pour formater des entrées utilisateur. Dans cet article, nous allons explorer comment effectuer cette conversion de manière efficace.

## Comment faire

Il existe plusieurs façons de convertir une chaîne en minuscules en PHP. Voici quelques exemples:

```PHP 
$string = "PROGRAMMATION EN PHP"; 
echo strtolower($string); 
// output: programmation en php
``` 

Dans cet exemple, nous utilisons la fonction `strtolower()` qui existe déjà en PHP. Elle prend une chaîne de caractères en entrée et renvoie la même chaîne en minuscules.

Une autre façon de réaliser cette conversion est d'utiliser la fonction `mb_strtolower()`, qui prend en charge les caractères multibytes dans les langues autres que l'anglais. Voici un exemple d'utilisation:

```PHP 
$string = "PROGRAMMATION EN PHP"; 
echo mb_strtolower($string); 
// output: programmation en php
``` 

Si vous souhaitez conserver la casse des lettres accentuées, vous pouvez utiliser la fonction `mb_convert_case()` avec le paramètre `MB_CASE_LOWER`. Voici comment cela fonctionne:

```PHP 
$string = "J'AIME LES FRAISES"; 
echo mb_convert_case($string, MB_CASE_LOWER, 'UTF-8'); 
// output: j'aime les fraises
``` 

## Plongée en profondeur

Il est important de noter que toutes ces fonctions de conversion de chaînes en minuscules prennent en compte la locale de votre système. Cela signifie que si votre système est configuré pour une langue autre que l'anglais, la conversion se fera en suivant les règles de casse de cette langue. Par exemple, si votre système est en français, la chaîne "PROGRAMMATION EN PHP" sera convertie en "programmation en php".

De plus, il est possible d'utiliser des expressions régulières pour effectuer cette conversion. Cela peut être utile si vous souhaitez ignorer certaines parties de la chaîne. Voici un exemple d'utilisation:

```PHP 
$string = "J'aime les fraises"; 
echo preg_replace('/(jaime)/', 'jAIME', $string); 
// output: jAIME les fraises
``` 

En utilisant une expression régulière, nous avons spécifié que seuls les caractères entre parenthèses doivent être convertis en minuscules.

## Voir aussi

- Documentation officielle de PHP sur la fonction [strtolower](https://www.php.net/manual/fr/function.strtolower.php)
- Documentation officielle de PHP sur la fonction [mb_strtolower](https://www.php.net/manual/fr/function.mb-strtolower.php)
- Documentation officielle de PHP sur la fonction [mb_convert_case](https://www.php.net/manual/fr/function.mb-convert-case.php)
- Tutoriel sur les [expressions régulières en PHP](https://www.php.net/manual/fr/book.pcre.php)