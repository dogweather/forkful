---
title:                "Extraction de sous-chaines"
html_title:           "Bash: Extraction de sous-chaines"
simple_title:         "Extraction de sous-chaines"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & pourquoi?
L'extraction de sous-chaînes est une technique utilisée par les programmeurs pour récupérer une partie spécifique d'une chaîne de caractères. Cela est utile lors de la manipulation de grandes quantités de données textuelles ou pour effectuer des opérations spécifiques sur certaines parties d'une chaîne. Les programmeurs utilisent souvent cette technique pour simplifier leur code et le rendre plus efficace.

## Comment faire:
Voici un exemple de code Bash pour extraire les deux premiers caractères d'une chaîne de caractères :

```
str = "Bonjour"
echo ${str:0:2}
```

La sortie de ce code serait "Bo", car les caractères aux positions 0 et 1 sont extraits de la chaîne originale. Vous pouvez également spécifier un index de départ différent en modifiant le nombre après le premier ":". Par exemple, si vous voulez extraire les deux derniers caractères d'une chaîne, vous pouvez utiliser ```${str: -2}```. Le " -2 " spécifie que nous voulons commencer à extraire à partir de l'avant-dernier caractère de la chaîne.

## Plongée en profondeur:
L'extraction de sous-chaînes existe depuis les premiers jours de la programmation. Elle est principalement utilisée dans les langages de programmation de type C, mais elle est également disponible dans Bash. Une alternative à l'extraction de sous-chaînes est l'utilisation de la commande ```cut```, qui permet de spécifier des délimiteurs pour extraire une partie d'une chaîne. Cependant, l'utilisation de la commande ```cut``` peut parfois être plus complexe que l'extraction de sous-chaînes.

Au niveau de l'implémentation, l'extraction de sous-chaînes se base sur la manipulation des indices de caractères dans une chaîne. Cela signifie qu'il est important de comprendre comment les indices sont numérotés dans votre langage de programmation pour effectuer une extraction précise.

## Voir aussi:
Pour en savoir plus sur l'extraction de sous-chaînes en Bash, vous pouvez consulter la documentation officielle de Bash ou des ressources en ligne telles que Bash 101. Vous pouvez également rechercher des tutoriels sur l'extraction de sous-chaînes dans d'autres langages de programmation pour comparer les différentes méthodes d'implémentation.