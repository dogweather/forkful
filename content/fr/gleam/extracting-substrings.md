---
title:                "Gleam: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est une fonctionnalité utile dans de nombreux projets de programmation. Cela vous permet de sélectionner et de manipuler des parties spécifiques d'une chaîne de caractères, ce qui peut être particulièrement pratique lors du traitement de données ou de la création de modèles de texte.

## Comment faire

Voici comment vous pouvez utiliser la fonction d'extraction de sous-chaînes en utilisant le langage de programmation Gleam :

```
Gleam |> string.sub("Bonjour le monde", 8, 12)
```

La sortie pour cet exemple serait "le monde", car nous avons sélectionné les caractères de la 8e à la 12e position dans la chaîne "Bonjour le monde". Vous pouvez également utiliser des variables pour spécifier les positions de début et de fin, ce qui peut rendre l'extraction de sous-chaînes plus dynamique.

```
let message = "Bonjour le monde"
let debut = 8
let fin = 12
Gleam |> string.sub(message, debut, fin)
```

La sortie serait toujours "le monde", mais vous pouvez maintenant changer les valeurs de début et de fin pour extraire différentes sous-chaînes.

## Plongée en profondeur

La fonction `string.sub` est basée sur les index de caractères, ce qui signifie qu'elle compte chaque caractère individuel pour déterminer sa position dans la chaîne. Cela peut sembler évident, mais il est important de le savoir lors de l'extraction de sous-chaînes, car les espaces et les caractères spéciaux comptent également comme des caractères. Par exemple, si vous avez une chaîne avec des espaces et que vous essayez d'extraire une sous-chaîne à partir d'un certain nombre de positions, vous devez prendre en compte les espaces pour obtenir le résultat souhaité.

En outre, la fonction `string.sub` renverra une erreur si les positions spécifiées sont en dehors de la plage de caractères de la chaîne donnée. Par exemple, si vous essayez d'extraire une sous-chaîne à partir de la 15e position d'une chaîne avec seulement 10 caractères, cela entraînera une erreur.

## Voir aussi

Pour plus d'informations sur la fonction `string.sub` et d'autres fonctionnalités de manipulation de chaînes en Gleam, consultez ces liens utiles :

- [Documentation officielle de Gleam](https://gleam.run/documentation/)
- [Tutoriel de Gleam sur la manipulation de chaînes](https://www.fusioncharts.com/blog/learning-gleam-manipulating-strings/)