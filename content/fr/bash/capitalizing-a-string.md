---
title:                "Mettre en majuscule une chaîne"
html_title:           "Bash: Mettre en majuscule une chaîne"
simple_title:         "Mettre en majuscule une chaîne"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# La capitalisation des chaînes de caractères en Bash

## Qu'est-ce que c'est et pourquoi?

La capitalisation d'une chaîne est l'opération qui consiste à transformer toutes les lettres en majuscules. Les développeurs l'utilisent pour harmoniser les entrées de l'utilisateur ou pour mettre en évidence certains textes.

## Comment faire:

Bash offre plusieurs façons de capitaliser une chaîne. En voici quelques exemples:

```Bash
# Utiliser ^ (Ceci mettra la première lettre en majuscule)
string="bonjour tout le monde"
echo "${string^}"
```
Sortie:
```Bash
Bonjour tout le monde
```

```Bash
# Utilisez ^^ (Cela va capitaliser toute la chaîne)
string="bonjour tout le monde"
echo "${string^^}"
```
Sortie:
```Bash
BONJOUR TOUT LE MONDE
```

## Plongée en profondeur:

Bien que Bash ne soit pas un langage traditionnellement utilisé pour manipuler des chaînes, ces fonctionnalités ont été ajoutées dans la version 4.0. 

Il existe d'autres alternatives pour la capitalisation si vous utilisez une version antérieure de Bash, ou si vous préférez une autre approche. Par exemple, vous pouvez utiliser 'tr':

```Bash
echo 'bonjour tout le monde' | tr '[:lower:]' '[:upper:]'
```

Cette façon d’opérer est plus ancienne et universelle, elle fonctionne sur des systèmes Unix plus anciens qui pourraient ne pas avoir une version récente de Bash.

## Voir également:

Pour un aperçu plus détaillé de la manipulation de chaînes dans Bash, consultez ces ressources:

- [GNU Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [String Operations in Bash](http://tldp.org/LDP/abs/html/string-manipulation.html)
- [Bash Guide for Beginners](https://www.tldp.org/LDP/Bash-Beginners-Guide/html/)