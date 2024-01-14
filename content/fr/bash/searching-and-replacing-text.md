---
title:                "Bash: Rechercher et remplacer du texte"
simple_title:         "Rechercher et remplacer du texte"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes dans la programmation Bash. Cela peut être utile pour corriger des erreurs, mettre à jour du code obsolète ou encore formater des données spécifiques. Apprendre à le faire efficacement peut vous faire gagner beaucoup de temps lors de l'écriture de scripts Bash.

## Comment faire

La commande « `sed` » est souvent utilisée pour rechercher et remplacer du texte dans Bash. Voyons un exemple pour comprendre comment cela fonctionne.

Supposons que nous avons un fichier texte contenant une liste de noms de personnes, chacun séparé par un espace. Nous souhaitons remplacer tous les prénoms par des initiales. Voici le contenu du fichier « names.txt »:

```
John Smith
Mary Johnson
David Williams
```

Nous pouvons utiliser la commande « `sed` » pour remplacer les prénoms par des initiales. Nous saisissons la commande suivante dans le terminal:

```
sed -i 's/\([A-Z]\)\w*\s/\1./g' names.txt
```

Analysons chaque élément de cette commande:

- `-i` indique à « `sed` » de modifier directement le fichier d'origine, au lieu de simplement afficher le résultat dans le terminal.
- `s` indique que nous allons effectuer une substitution.
- `\([A-Z]\)\w*\s` est le motif destiné à être remplacé. Il correspond à une lettre majuscule, suivie de zéro ou plusieurs caractères alphanumériques (les prénoms), puis un espace.
- `\1.` est le remplacement. Il correspond au premier groupe de caractères que nous avons capturé dans le motif (la première lettre du prénom), suivi d'un point.
- `g` indique à « `sed` » de remplacer toutes les occurrences du motif, et pas seulement la première.

Et voici le résultat obtenu dans le fichier « names.txt » après avoir exécuté cette commande:

```
J. Smith
M. Johnson
D. Williams
```

Nous avons maintenant des initiales à la place des prénoms. Vous pouvez essayer d'autres options et motifs pour expérimenter avec « `sed` » et trouver ce qui fonctionne le mieux pour votre cas d'utilisation.

## Approfondissement

Bien que la commande « `sed` » soit très utile pour la recherche et le remplacement de texte, il existe d'autres moyens de le faire en Bash. Vous pouvez utiliser les commandes « `awk` », « `grep` » ou encore des opérations avec des expressions régulières intégrées dans un script Bash.

Il est également important de comprendre comment fonctionnent les modèles et les expressions régulières pour effectuer des recherches plus complexes. Vous pouvez consulter le manuel de référence officiel de Bash pour en savoir plus sur l'utilisation de ces outils.

## Voir aussi

- [Manuel de référence de Bash](https://www.gnu.org/software/bash/manual/)
- [Guide de la commande « sed »](https://www.gnu.org/software/sed/manual/sed.html)
- [Guide de la commande « awk »](https://www.gnu.org/software/gawk/manual/)
- [Guide de la commande « grep »](https://www.gnu.org/software/grep/manual/)