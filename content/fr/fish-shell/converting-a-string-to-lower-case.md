---
title:                "Convertir une chaîne en minuscules"
html_title:           "Arduino: Convertir une chaîne en minuscules"
simple_title:         "Convertir une chaîne en minuscules"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
En programmation, la conversion d'une chaîne de caractères en minuscules est un processus consistant à changer tous les caractères en majuscules d'une chaîne en leurs équivalents minuscules. Les programmeurs font cela souvent pour normaliser les données en vue d'une comparaison non sensible à la casse.

## Comment faire:
Voici comment vous pouvez convertir une chaîne en minuscules dans Fish Shell:

```Fish Shell 
set chaine "Voila, MON TEST"
echo $chaine | string lower
```

L'exemple de code ci-dessus produira la sortie suivante:

```Fish Shell 
voila, mon test
```

## Plongée profonde
Historiquement, la possibilité de convertir des chaînes en minuscules, a été d'une grande utilité dans de nombreux contextes de programmation, surtout lors de la manipulation de textes ou de fichiers textuels.

Il existe plusieurs alternatives pour convertir une chaîne en minuscules. Par exemple, en utilisant `tr` dans Unix comme ci-dessous :

```Fish Shell 
echo $chaine | tr '[:upper:]' '[:lower:]'
```

La commande `string lower` dans Fish Shell est une implémentation interne qui utilise l'API C++ std::tolower pour faire la conversion. Elle traite chaque caractère de la chaîne, un par un, en le convertissant en minuscule si c'est une lettre majuscule.

## Voir aussi
Pour plus d'informations sur les fonctions de chaîne fournies par Fish Shell, vous pouvez consulter la page officielle de la documentation:

- [Documentation Fish Shell](https://fishshell.com/docs/current/commands.html#string)

Pour une information plus profonde sur la fonction std::tolower, consultez:

- [Documentation std::tolower](https://en.cppreference.com/w/cpp/string/byte/tolower) 

Note: Les liens sont en anglais, car il s'agit des sources officielles.