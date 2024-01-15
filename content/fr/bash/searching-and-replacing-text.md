---
title:                "Recherche et remplacement de texte"
html_title:           "Bash: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous devez modifier plusieurs occurrences d'un mot ou d'une phrase dans un document ou un fichier, il serait fastidieux de le faire manuellement. Heureusement, Bash offre une solution simple et rapide pour effectuer des recherches et des remplacements de texte en utilisant des commandes intégrées.

## Comment faire

Pour effectuer une recherche et un remplacement de texte en utilisant Bash, vous pouvez utiliser la commande `sed` suivie des options et des expressions régulières nécessaires. Par exemple, si vous voulez remplacer toutes les occurrences du mot "chat" par "chien" dans un fichier appelé "animaux.txt", vous pouvez utiliser la commande suivante :

```Bash
sed -i 's/chat/chien/g' animaux.txt
```

Cette commande recherche toutes les occurrences du mot "chat" dans le fichier "animaux.txt" et les remplace par "chien". L'option `-i` permet de modifier directement le fichier, sans créer de nouveau fichier de sortie.

Vous pouvez également utiliser des expressions régulières pour un remplacement plus précis. Par exemple, si vous voulez remplacer toutes les occurrences de "chat" par "chien" uniquement s'il est précédé d'un espace, vous pouvez utiliser l'expression régulière `\schat` :

```Bash
sed -i 's/\schat/chien/g' animaux.txt
```

## Plongée en profondeur

La commande `sed` utilise des expressions régulières pour correspondre à des motifs de texte spécifiques et effectuer des modifications en conséquence. Vous pouvez utiliser une variété d'options et de symboles pour personnaliser votre recherche et remplacement, tels que :

- `g` pour remplacer toutes les occurrences d'un motif dans chaque ligne
- `i` pour ignorer la casse lors de la recherche
- `p` pour afficher la ligne modifiée
- `d` pour supprimer la ligne qui correspond au motif

Vous pouvez également utiliser des expressions régulières étendues grâce à l'option `-E`, qui vous permet d'utiliser des caractères spéciaux comme `+`, `*` et `()` dans vos motifs de recherche.

Pour en savoir plus sur les fonctionnalités avancées de la commande `sed` et l'utilisation des expressions régulières pour les recherches et les remplacements, consultez la [documentation officielle de Bash](https://www.gnu.org/software/sed/manual/sed.html).

## Voir aussi

- [Documentation officielle de Bash](https://www.gnu.org/software/bash/manual/bash.html)
- [Guide de référence Bash pour débutants](https://www.apprendre-gnulinux.com/sections/environnement-shell/bash)
- [Tutoriel sur les expressions régulières en Bash](https://www.digitalocean.com/community/tutorials/the-basics-of-using-the-sed-stream-editor-to-manipulate-text-in-linux-fr)