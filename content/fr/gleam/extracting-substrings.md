---
title:                "Extraction de sous-chaînes"
html_title:           "Gleam: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

Qu'est-ce que l'extraction de sous-chaînes et pourquoi les programmeurs le font-ils?

L'extraction de sous-chaînes est un processus qui implique de prendre une partie d'une chaîne de caractères plus grande et de la récupérer en tant que nouvelle chaîne autonome. Les programmeurs le font souvent lorsqu'ils ont besoin d'extraire des données spécifiques d'une chaîne plus grande, telles que des numéros de téléphone, des adresses électroniques ou des codes postaux.

Comment faire:

Voici un exemple de code en Gleam montrant comment extraire une sous-chaîne à partir d'une chaîne donnée:

```Gleam
let message = "Bonjour tout le monde"
let salutation = message->[0,7]
```
Dans cet exemple, nous utilisons le symbole `->` pour extraire les caractères de la position 0 à 7 de la chaîne "Bonjour tout le monde". La variable `salutation` contiendra la sous-chaîne "Bonjour". Vous pouvez également utiliser l'opérateur `[start,end]` pour spécifier plus précisément la partie de la chaîne que vous souhaitez extraire.

 Plongée profonde:
 
L'extraction de sous-chaînes peut sembler un concept simple, mais elle est en fait très utile pour de nombreuses tâches de programmation. Elle a été initialement développée pour faciliter la manipulation de chaînes de caractères dans les langages de programmation comme C et Java. Bien qu'il existe maintenant des alternatives telles que les expressions régulières, l'extraction de sous-chaînes reste un moyen simple et efficace de manipuler des données dans de nombreux cas.

Voir aussi:

Si vous souhaitez en savoir plus sur l'extraction de sous-chaînes en Gleam, voici quelques liens utiles:

- La documentation officielle de Gleam sur les chaînes de caractères: https://gleam.run/docs/std/string
- Un tutoriel vidéo pour apprendre à manipuler les chaînes de caractères en Gleam: https://www.youtube.com/watch?v=dQw4w9WgXcQ
- Un article de blog sur les différentes façons d'extraire des sous-chaînes en programmation: https://www.codecademy.com/articles/substring