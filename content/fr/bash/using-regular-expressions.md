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

## Qu'est-ce que c'est et pourquoi les développeurs l'utilisent-ils ?
Les expressions régulières sont des séquences de caractères utilisées pour rechercher et manipuler des chaînes de caractères. Les développeurs les utilisent pour effectuer des opérations telles que la validation de données, l'extraction d'informations ou la manipulation de fichiers. Cela permet d'automatiser des tâches récurrentes et de gagner du temps lors de la manipulation de données.

## Comment faire :
Les expressions régulières peuvent être utilisées dans le terminal avec des commandes Bash telles que `grep`, `sed` et`awk`. Par exemple, pour trouver tous les fichiers contenant le mot "bonjour", vous pouvez utiliser la commande suivante :
```Bash
grep -l "bonjour" *
```
Cela affichera tous les fichiers contenant le mot "bonjour" dans leur nom ou leur contenu.

## Plongez dans les détails :
Les expressions régulières ont été développées dans les années 1950 et ont été popularisées dans les années 1970 par le langage de programmation Perl. Il existe également des alternatives comme les expressions rationnelles, qui sont plus simples à utiliser mais offrent moins de flexibilité. L'implémentation des expressions régulières implique l'utilisation d'opérateurs et de constructions spécifiques, tels que `*` pour représenter un nombre quelconque de caractères, ou `[]` pour représenter une classe de caractères.

## Voir aussi :
Pour en savoir plus sur les expressions régulières et leur utilisation dans Bash, consultez la documentation officielle de GNU `grep` (https://www.gnu.org/software/grep/manual/grep.html#Regular-Expressions) et le tutoriel Bash du site LinuxCommand.org (https://linuxcommand.org/lc3_adv_les8.php). Vous pouvez également trouver des listes complètes d'opérateurs et de constructions dans la documentation en ligne de Perl (https://perldoc.perl.org/perlre.html) et de Python (https://docs.python.org/fr/3/library/re.html).