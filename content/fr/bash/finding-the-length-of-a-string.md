---
title:                "Trouver la longueur d'une chaîne"
html_title:           "Go: Trouver la longueur d'une chaîne"
simple_title:         "Trouver la longueur d'une chaîne"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?

Trouver la longueur d'une chaîne signifie déterminer le nombre de caractères qu'elle contient. C'est une opération courante en programmation, notamment pour la validation des entrées ou le traitement des données textuelles.

## Comment faire:

Voici un exemple simple de comment trouver la longueur d'une chaîne en Bash :

```bash
chaine="Bonjour, monde!"
echo ${#chaine}
```
Sortie attendue: 15.

## Plongée Profonde:

Historiquement, Bash n’offrait aucun moyen direct de trouver la longueur d’une chaîne. Au fil du temps, le moyen d'utiliser les accolades pour obtenir la longueur d'une chaîne a été introduit.

Il existe d'autres moyens pour atteindre le même objectif en Bash, bien que la méthode précédente soit la plus couramment utilisée. Par exemple, en utilisant la commande `expr` :

```bash
chaine="Bonjour, monde!"
echo `expr length "$chaine"`
```

Le détail d'implémentation de trouver la longueur de la chaîne en Bash est assez simple. Bash interprète le texte entre accolades comme une instruction. L'opération `#` est exécuté sur la chaîne, ce qui retourne sa longueur.

## À Voir Aussi:

Vous pouvez consulter d'autres ressources liées à Bash et aux chaînes de caractères, pour approfondir vos connaissances :

1. [GNU Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
2. [Bash String Manipulations Cheat Sheet](https://devhints.io/bash)
3. [Learn Bash in Y minutes](https://learnxinyminutes.com/docs/bash/)