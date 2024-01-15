---
title:                "Utiliser les expressions régulières"
html_title:           "Bash: Utiliser les expressions régulières"
simple_title:         "Utiliser les expressions régulières"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi 

Utiliser les expressions régulières dans Bash peut grandement faciliter le traitement de données et la manipulation de fichiers. Cela peut également vous permettre d'automatiser certaines tâches répétitives plus rapidement et efficacement.

## Comment faire 

Les expressions régulières dans Bash sont définies entre deux 'slash' (/) et peuvent être utilisées avec des commandes comme 'grep', 'sed' ou encore 'awk'. Voici quelques exemples de code pour vous montrer leur efficacité :

```Bash
# Trouver toutes les lignes contenant des mots commençant par la lettre 'a'
grep '/a[a-z]/' fichier.txt

# Remplacer toutes les occurrences du mot 'hello' par 'bonjour' dans un fichier
sed 's/hello/bonjour/g' fichier.txt

# Afficher toutes les lignes commençant par 'b' et contenant 3 caractères
awk '/^b.{2}/' fichier.txt
```

Les expressions régulières peuvent également être combinées avec des opérateurs logiques tels que '|' (ou), '&' (et) ou encore '!' (non) pour effectuer des recherches plus complexes.

## Plongée en profondeur 

En plus des commandes mentionnées, les expressions régulières peuvent être utilisées dans d'autres contextes tels que le renommage de fichiers, la recherche dans des chaînes de caractères ou encore la validation de formats.

Il existe également de nombreux modificateurs tels que 'i' (ignore case), 'm' (multi-line) ou encore 'g' (global) qui peuvent être ajoutés à une expression régulière pour effectuer des recherches plus précises.

## Voir aussi 

- [La référence Bash complète des expressions régulières](https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html)
- [Tutoriel Bash : Les expressions régulières](https://www.linuxtricks.fr/wiki/bash-les-expressions-regulieres)