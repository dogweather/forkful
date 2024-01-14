---
title:                "PHP: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Vous êtes peut-être déjà familiarisé avec les chaînes de caractères en PHP, mais saviez-vous qu'il existe un moyen plus efficace de rechercher et manipuler ces chaînes? Les expressions régulières, également connues sous le nom de regex, sont un outil puissant pour rechercher des motifs dans les chaînes de caractères. Dans cet article, nous allons explorer pourquoi les regex sont utiles et comment les utiliser dans vos projets PHP.

## Comment faire

Pour utiliser les expressions régulières en PHP, vous devez d'abord créer un modèle qui spécifie le motif à rechercher. Par exemple, si vous souhaitez trouver tous les numéros de téléphone dans une chaîne, vous pouvez utiliser le modèle suivant: ```PHP /[0-9]{3}-[0-9]{3}-[0-9]{4}/ ``` Ce modèle spécifie que la chaîne doit contenir 3 chiffres, suivis d'un tiret, puis 3 autres chiffres, et enfin 4 chiffres supplémentaires.

Maintenant que vous avez un modèle, vous pouvez l'utiliser avec la fonction PHP ```preg_match()```. Cette fonction prend deux arguments: le modèle et la chaîne dans laquelle vous souhaitez rechercher. Si des correspondances sont trouvées, elles seront stockées dans un tableau. Voici un exemple de code:

```PHP
<?php
$chaine = "Mon numéro de téléphone est 123-456-7890.";
$modele = "/[0-9]{3}-[0-9]{3}-[0-9]{4}/";
preg_match($modele, $chaine, $correspondances);
print_r($correspondances);
?>
```

Le code ci-dessus devrait produire la sortie suivante:

```
Array
(
    [0] => 123-456-7890
)
```

Vous pouvez également utiliser les expressions régulières pour remplacer des parties d'une chaîne de caractères. Par exemple, si vous voulez remplacer tous les tirets dans un numéro de téléphone par des espaces, vous pouvez utiliser la fonction PHP ```preg_replace()```:

```PHP
<?php
$chaine = "Mon numéro de téléphone est 123-456-7890.";
$modele = "/[0-9]{3}-[0-9]{3}-[0-9]{4}/";
$nouvelle_chaine = preg_replace($modele, "123 456 7890", $chaine);
echo $nouvelle_chaine;
?>
```

Le code ci-dessus devrait produire la sortie suivante:

```
Mon numéro de téléphone est 123 456 7890.
```

## Plongeon en profondeur

Les expressions régulières peuvent sembler intimidantes au début, mais une fois que vous les avez maîtrisées, elles peuvent grandement améliorer vos compétences en programmation. Une chose importante à noter est que les expressions régulières peuvent être spécifiques à un langage de programmation, vous devez donc vous assurer d'utiliser la syntaxe correcte pour PHP. De plus, il peut être utile d'utiliser des sites tels que [Regex101](https://regex101.com/) pour tester vos modèles avant de les utiliser dans votre code.

Un autre avantage des regex est qu'ils sont très rapides pour traiter de grandes quantités de données. Si vous avez un projet qui nécessite la recherche et la manipulation de chaînes de caractères, utiliser des expressions régulières pourrait vous faire gagner un temps précieux.

Enfin, il existe de nombreuses ressources en ligne, telles que des tutoriels et des forums, pour vous aider à apprendre et à perfectionner vos compétences en expressions régulières. Alors n'hésitez pas à plonger en profondeur et à découvrir tout ce que les regex ont à offrir!

## Voir aussi

- [Documentation officielle de PHP sur les expressions régulières](https://www.php.net/manual/fr/reference.pcre.pattern.syntax.php)
- [Tutoriel vidéo sur les expressions régulières en PHP](https://www.youtube.com/watch?v=r_6I6m2F7hk)
- [Regex101 - Tester et expérimenter avec des modèles regex en ligne](https://regex101.com/)