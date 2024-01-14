---
title:    "Bash: Lecture d'un fichier texte"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire un fichier texte peut sembler être une tâche simple et banale pour la plupart des programmeurs. Pourtant, il est important de comprendre comment le faire efficacement en utilisant Bash dans vos projets de programmation. Cela peut vous faire gagner du temps et améliorer la qualité de votre code.

## Comment le faire

Heureusement, lire un fichier texte en Bash est simple et ne nécessite que quelques lignes de code. Tout d'abord, vous devez ouvrir votre fichier avec la commande "cat" suivie du nom du fichier :

```Bash
cat nom_du_fichier.txt
```

Cela affichera le contenu du fichier dans la console. Mais si vous souhaitez stocker le contenu dans une variable, vous pouvez utiliser la redirection ">" pour écrire le contenu dans une nouvelle variable :

```Bash
contenu=$(cat nom_du_fichier.txt)
```

De plus, il est possible de lire ligne par ligne en utilisant la commande "read" :

```Bash
while read ligne; do
    echo $ligne
done < nom_du_fichier.txt
```

Cela vous permet de traiter chaque ligne du fichier individuellement. N'oubliez pas que les lignes sont séparées par des retours à la ligne.

## Plongée en profondeur

Il est important de noter que la commande "cat" peut également être utilisée pour concaténer plusieurs fichiers ensemble. Par exemple, si vous souhaitez combiner le contenu de deux fichiers dans un nouveau fichier :

```Bash
cat fichier_1.txt fichier_2.txt > nouveau_fichier.txt
```

De plus, la commande "grep" peut être utilisée pour rechercher un terme spécifique dans un fichier. Cela peut être utile si vous recherchez un mot clé ou une information précise.

```Bash
grep "mot_clé" nom_du_fichier.txt
```

N'hésitez pas à explorer d'autres commandes Bash pour lire et traiter des fichiers texte de différentes manières.

## Voir aussi

- [Guide de référence Bash](https://www.gnu.org/software/bash/manual/)
- [10 commandes Bash utiles pour les débutants](https://www.tutorialspoint.com/unix/unix-useful-commands.htm)
- [Leçon interactive de commandes Bash](https://www.learnshell.org/)