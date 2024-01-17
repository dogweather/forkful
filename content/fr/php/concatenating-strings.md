---
title:                "Concaténation de chaînes de caractères"
html_title:           "PHP: Concaténation de chaînes de caractères"
simple_title:         "Concaténation de chaînes de caractères"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

En programmation, la concaténation de chaînes de caractères est l'action de fusionner deux ou plusieurs chaînes de caractères en une seule. Les programmeurs le font principalement pour créer du texte dynamique en combinant des variables et des valeurs avec du texte fixe.

## Comment le faire:

La concaténation de chaînes de caractères en PHP est simple et se fait en utilisant l'opérateur de concaténation "." (point). Voici un exemple:

```PHP
$nom = "Jean";
$age = 25;
echo "Bonjour, je m'appelle " . $nom . " et j'ai " . $age . " ans.";
```

Cela affichera:

Bonjour, je m'appelle Jean et j'ai 25 ans.

Vous pouvez également concaténer des chaînes de caractères en utilisant la fonction ```sprintf``` qui vous permet de spécifier un format pour votre chaîne de caractères finale. Par exemple:

```PHP
$nom = "Marie";
$age = 30;
$texte = sprintf("Bonjour, je m'appelle %s et j'ai %d ans.", $nom, $age);
echo $texte;
```

Cela affichera le même résultat que l'exemple précédent. Cependant, la fonction ```sprintf``` peut être utile si vous avez besoin de formater votre chaîne de caractères d'une certaine manière.

## Plongée en profondeur:

La concaténation de chaînes de caractères est une technique couramment utilisée en programmation et elle peut avoir un impact positif sur les performances de votre code par rapport à d'autres alternatives telles que l'utilisation de la fonction ```implode``` ou la construction de chaînes HTML avec des variables.

L'utilisation de l'opérateur de concaténation "." peut sembler fastidieuse lorsque vous devez concaténer de nombreuses chaînes de caractères, cependant, cela peut être contourné en utilisant la notation d'hébergement soutenue par PHP:

```PHP
echo "Bonjour, je m'appelle $nom et j'ai $age ans.";
```

Cela revient au même que l'exemple précédent, mais vous n'avez pas besoin d'utiliser l'opérateur de concaténation à chaque fois.

Enfin, il est important de noter que la concaténation de chaînes de caractères peut également être effectuée sur des tableaux en utilisant la fonction ```implode```. Cela peut être utile si vous avez besoin de fusionner plusieurs éléments d'un tableau en une seule chaîne de caractères.

## Voir aussi:

- [Documentation officielle de PHP sur la concaténation de chaînes de caractères](https://www.php.net/manual/fr/language.operators.string.php)
- [Tutoriel sur la concaténation de chaînes de caractères en PHP](https://www.tutorialspoint.com/php/php_string_concatenation.htm)
- [Vidéo explicative sur la concaténation de chaînes de caractères en PHP](https://www.youtube.com/watch?v=Mf_qBb_i0LI)