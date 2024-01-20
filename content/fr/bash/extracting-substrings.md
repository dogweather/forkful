---
title:                "Extraction de sous-chaînes"
html_title:           "Arduino: Extraction de sous-chaînes"
simple_title:         "Extraction de sous-chaînes"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'extraction de sous-chaînes signifie obtenir des parties spécifiques d'une chaîne de caractères. Les programmeurs le font souvent pour manipuler, analyser, ou substituer certaines portions d'une chaîne.

## Comment Faire:

Vous pouvez extraire des sous-chaînes dans Bash avec ${var:position:longueur}. Voici un exemple:

```Bash
ma_chaine="Bonjour le monde"
echo ${ma_chaine:0:7}   # sortie: Bonjour
echo ${ma_chaine:8:2}   # sortie: le
```

Ici, nous avons défini 'position' comme un index basé sur 0, et 'longueur' comme le nombre de caractères à extraire.

## Plongée Profonde:

Historiquement, Bash a hérité de la fonctionnalité d'extraction de sous-chaînes de l'interpréteur de commandes 'sh'. Aujourd'hui, c'est une fonctionnalité précieuse pour les scripts bash, mais il existe aussi des alternatives. Par exemple, on peut aussi utiliser 'cut' ou 'awk', deux autres commandes Unix.

Notez que les indices de position sont basés sur zéro. Aussi, si 'longueur' dépasse la longueur restante de la chaîne, Bash ne se plaint pas et renvoie simplement la portion restante de la chaîne.

## Voir Aussi:

Pour plus d'informations sur l'extraction de sous-chaînes en Bash, consultez ces sources :

- Le manuel Bash : https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- IBM Developer : https://developer.ibm.com/technologies/linux/tutorials/l-bash-strings/
- StackOverflow : https://stackoverflow.com/questions/428109/extract-substring-in-bash