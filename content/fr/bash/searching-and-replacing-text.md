---
title:    "Bash: Rechercher et remplacer du texte"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont deux opérations courantes lors de la programmation en Bash. Ces fonctionnalités vous permettent de modifier facilement du texte dans un fichier, un répertoire ou encore une variable. Que vous soyez un développeur débutant ou expérimenté, apprendre à utiliser la recherche et le remplacement de texte en Bash peut grandement améliorer votre efficacité lors de la programmation.

## Comment Faire

La syntaxe de base pour la recherche et le remplacement de texte en Bash est la suivante :

```Bash
sed 's/pattern/replacement/g' input_file > output_file
```

Cette commande utilise l'outil de ligne de commande "sed" pour rechercher le motif spécifié dans le fichier d'entrée (input_file) et le remplacer par le texte spécifié (replacement) dans le fichier de sortie (output_file). L'option "g" à la fin de la commande indique à Bash de remplacer toutes les occurrences du motif, et non seulement la première.

Par exemple, si nous voulons remplacer toutes les occurrences de "chat" par "chien" dans un fichier texte appelé "animaux.txt", nous pouvons utiliser la commande suivante :

```Bash
sed 's/chat/chien/g' animaux.txt > nouveaux_animaux.txt
```

Le fichier de sortie, "nouveaux_animaux.txt", contiendra toutes les mêmes lignes que "animaux.txt", mais avec le mot "chien" à la place de "chat" chaque fois qu'il apparaît.

## Plongée en Profondeur

Bash offre de nombreuses fonctions utiles pour la recherche et le remplacement de texte, telles que l'utilisation de variables pour spécifier les motifs ou les remplacements, ou encore l'utilisation d'expressions régulières pour des recherches plus spécifiques. Vous pouvez également combiner différentes commandes Bash pour créer des scripts complexes pour automatiser la recherche et le remplacement de texte dans plusieurs fichiers ou répertoires à la fois.

Il est important de comprendre que la commande "sed" modifie uniquement le texte dans le fichier de sortie, et non le fichier d'entrée d'origine. Si vous voulez que les modifications soient apportées directement dans le fichier d'entrée, vous pouvez utiliser l'option "-i" comme ceci :

```
sed -i 's/pattern/replacement/g' input_file
```

Enfin, si vous voulez simplement visualiser les modifications sans les enregistrer dans un nouveau fichier, vous pouvez utiliser l'option "-n" pour désactiver l'affichage automatique des lignes, et utiliser l'option "p" pour spécifier quelles lignes vous voulez visualiser. Par exemple, pour voir les lignes où le mot "pomme" a été remplacé par "banane" dans le fichier "fruit.txt", vous pouvez utiliser la commande suivante :

```
sed -n 's/pomme/banane/p' fruit.txt
```

## Voir Aussi

- [Documentation officielle de Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Tutoriel pour lire et écrire dans des fichiers en Bash](https://www.hostinger.fr/tutoriels/fichiers-bash-lecture-ecriture/)
- [Guide complet pour les expressions régulières en Bash](https://www.cyberciti.biz/faq/guide-to-bash-regular-expressions-regex/)