---
title:    "Fish Shell: Conversion d'une chaîne en minuscules"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Pourquoi

La conversion d'une chaîne de caractères en lettres minuscules est une tâche courante en programmation. Cela peut être utile pour normaliser les données entrées par les utilisateurs ou pour faciliter les comparaisons de chaînes de caractères.

## Comment faire

Voici un exemple de code en utilisant Fish Shell pour convertir une chaîne en lettres minuscules :

```
set string "Bonjour le Monde"
set lower_string (string | tr '[:upper:]' '[:lower:]')
echo $lower_string
```

Le résultat de cet exemple sera :

```
bonjour le monde
```

## Plongée en profondeur

Il existe plusieurs façons de convertir une chaîne de caractères en lettres minuscules en utilisant Fish Shell. Vous pouvez utiliser la commande `string`, comme dans l'exemple ci-dessus, ou utiliser les commandes `lower` ou `tolower` selon vos préférences. Vous pouvez également utiliser des expressions régulières pour effectuer cette conversion.

La conversion en lettres minuscules peut également être effectuée sur des chaînes de caractères contenant des caractères spéciaux tels que des accents. Fish Shell offre une prise en charge Unicode complète, ce qui en fait un outil pratique pour gérer des chaînes de caractères multilingues.

## Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Page Wikipedia sur les commandes convertissant la casse des chaînes](https://fr.wikipedia.org/wiki/Liste_de_commandes_concernant_la_casse)