---
title:    "Bash: Lecture d'un fichier texte"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur débutant en Bash, alors vous avez probablement entendu parler de la lecture et de l'écriture de fichiers texte. Mais pourquoi est-il important de comprendre comment lire un fichier texte ? Eh bien, la réponse est simple : la lecture de fichiers texte est une compétence de base qui vous permettra de manipuler et d'analyser des données dans vos scripts Bash. Cela vous permettra également d'automatiser des tâches et de gagner du temps dans votre travail quotidien.

## Comment faire

Maintenant que vous comprenez pourquoi la lecture de fichiers texte est importante, il est temps de voir comment le faire dans Bash. Tout d'abord, vous devez vous assurer que vous êtes dans le bon répertoire où se trouve le fichier texte que vous souhaitez lire. Ensuite, vous pouvez utiliser la commande "cat" pour afficher le contenu du fichier texte. Par exemple :

```Bash
cat exemple.txt
```

Cela affichera tout le contenu du fichier texte sur votre terminal. Mais vous pouvez également vouloir enregistrer le contenu dans une variable pour le manipuler dans votre script. Pour ce faire, vous pouvez utiliser la commande "read". Par exemple :

```Bash
read contenu < exemple.txt
```

Cela enregistrera le contenu du fichier texte dans la variable "contenu" pour que vous puissiez le traiter davantage. Vous pouvez également spécifier une certaine partie du fichier à lire en utilisant des opérateurs de redirections tels que ">" ou "|". Par exemple :

```Bash
cat exemple.txt | head -n 5 > cinq_lignes.txt
```

Cela ne lira que les 5 premières lignes du fichier "exemple.txt" et les enregistrera dans un nouveau fichier "cinq_lignes.txt".

## Plongée en profondeur

Maintenant que vous savez comment lire un fichier texte en utilisant Bash, il est important de comprendre quelques concepts plus avancés. Tout d'abord, il est important de noter que le contenu d'un fichier texte peut être manipulé en utilisant des expressions régulières ou des commandes telles que "grep" pour rechercher un mot spécifique dans le fichier. Deuxièmement, dans certains cas, il peut être utile d'itérer sur chaque ligne du fichier pour effectuer des traitements spécifiques. Pour ce faire, vous pouvez utiliser une boucle while avec la commande "read" mentionnée précédemment. Enfin, il est également important de noter que vous pouvez écrire dans un fichier texte en utilisant la commande "echo" ou "printf". Par exemple :

```Bash
echo "Bonjour le monde !" > salutations.txt
```

Cela écrira la phrase "Bonjour le monde !" dans un nouveau fichier "salutations.txt".

## Voir aussi

Si vous souhaitez en savoir plus sur la manipulation de fichiers texte en Bash, voici quelques liens utiles :

- [Documentation officielle de Bash](https://www.gnu.org/software/bash/)
- [Un guide complet sur la manipulation de fichiers en Bash](https://linuxhint.com/bash_read_file/)
- [Tutoriel vidéo sur la lecture et l'écriture de fichiers texte en Bash](https://www.youtube.com/watch?v=Q6hvxzfsV8c)