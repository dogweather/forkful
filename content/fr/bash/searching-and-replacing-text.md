---
title:                "Rechercher et remplacer du texte"
html_title:           "Bash: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

La recherche et le remplacement de texte est une pratique courante chez les programmeurs pour modifier rapidement et efficacement du texte dans un fichier ou un ensemble de fichiers. Cela permet de remplacer automatiquement des mots ou des phrases par d'autres sans avoir à le faire manuellement.

## Comment faire:

Voici un exemple simple de recherche et remplacement de texte en utilisant la commande ```sed``` en Bash:

```Bash
sed -i 's/ancien_mot/nouveau_mot/g' fichier.txt
```
Cette commande remplacera toutes les occurrences de "ancien_mot" par "nouveau_mot" dans le fichier "fichier.txt". Notez que l'option "-i" modifie directement le fichier sans créer de nouveau fichier.

## Plongée Profonde:

Cette technique de recherche et remplacement de texte n'est pas nouvelle et a été utilisée dans les premiers langages de programmation tels que le langage de manipulation de chaînes SNOBOL en 1962. Dans Bash, il existe également d'autres méthodes pour effectuer des recherches et remplacements de texte, notamment en utilisant la commande ```grep```, le langage de script awk ou encore la fonction intégrée ```[[pattern]=replacement]]```.

## Voir aussi:

- Guide de référence pour les commandes Bash: https://devhints.io/bash
- Manuel de référence de la commande sed: https://www.gnu.org/software/sed/manual/sed.html
- Tutoriel sur l'utilisation des expressions régulières en Bash: https://www.2daygeek.com/regular-expression-regex-examples-with-sed-command-in-linux/