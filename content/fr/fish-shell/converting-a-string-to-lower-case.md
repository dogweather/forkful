---
title:                "Convertir une chaîne en minuscules."
html_title:           "Fish Shell: Convertir une chaîne en minuscules."
simple_title:         "Convertir une chaîne en minuscules."
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi les programmeurs le font?

Les programmeurs utilisent souvent la fonction de conversion en minuscules pour modifier le format d'une chaîne de caractères. Cela signifie simplement que toutes les lettres de la chaîne seront transformées en minuscules, peu importe leur forme d'origine. Cette manipulation est couramment utilisée pour simplifier les comparaisons de chaînes de caractères, en ignorant les variations de casse.

# Comment le faire:

Voici un exemple de code en Fish Shell qui utilise la fonction de conversion en minuscules:

```
set myString "Bonjour Tout Le Monde"
echo $myString | tr '[:upper:]' '[:lower:]'
```

La sortie de ce code sera ```bonjour tout le monde```, avec toutes les lettres en minuscules. Nous utilisons la commande  ```tr``` (pour "translate"), qui permet de remplacer les caractères dans une chaîne par d'autres caractères.

# Analyse approfondie:

## Contexte historique:
La commande ```tr``` est une fonctionnalité standard disponible sur les systèmes d'exploitation Unix depuis les années 1970. Elle a été créée pour faciliter la manipulation de chaînes de caractères et est toujours largement utilisée par les programmeurs pour de nombreuses tâches, y compris la conversion en minuscules.

## Alternatives:
Il existe de nombreuses façons de convertir une chaîne en minuscules, selon le langage de programmation que vous utilisez. Par exemple, en Python, vous pouvez utiliser la fonction ```lower()```, tandis qu'en Java, vous pouvez utiliser ```toLowerCase()```.

## Détails de l'implémentation:
La commande ```tr``` utilise un algorithme simple pour convertir une chaîne en minuscules : elle parcourt simplement chaque caractère et le remplace par sa version en minuscules, en utilisant les règles définies dans l'alphabet de la langue sélectionnée.

# Voir aussi:

Pour en savoir plus sur la manipulation de chaînes de caractères en Fish Shell, vous pouvez consulter la documentation officielle sur les commandes de manipulation de chaînes: https://fishshell.com/docs/current/cmds/set.html#commands-string