---
title:                "Lecture d'un fichier texte"
html_title:           "Bash: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Tu as peut-être entendu parler de Bash en tant que "langage de script" pour automatiser des tâches sur ton système d'exploitation. Mais savais-tu qu'il est également un outil utile pour manipuler des fichiers de texte ? Dans cet article, je vais t'expliquer comment lire un fichier texte en utilisant Bash.

## Comment faire

Tout d'abord, nous avons besoin d'un fichier texte à lire. Crée un fichier nommé "monfichier.txt" dans ton répertoire de travail et ajoute quelques lignes de texte à l'intérieur.

Maintenant, ouvre ton terminal et navigue jusqu'à ton répertoire de travail. Nous allons utiliser la commande "cat" pour afficher le contenu de notre fichier.

```bash
cat monfichier.txt
```

Tu verras le contenu de ton fichier s'afficher dans le terminal. Mais que faire si tu veux afficher seulement une partie du fichier ? Utilisons la commande "head" pour n'afficher que les premières lignes.

```bash
head monfichier.txt
```

Ensuite, nous pouvons utiliser la commande "tail" pour afficher les dernières lignes du fichier.

```bash
tail monfichier.txt
```

Et si tu veux rechercher un mot spécifique dans ton fichier texte ? Nous pouvons utiliser la commande "grep" pour cela. Par exemple, si tu veux rechercher toutes les lignes contenant le mot "Bash" dans ton fichier, tu peux utiliser la commande suivante :

```bash
grep Bash monfichier.txt
```

Il existe de nombreuses autres commandes et options pour lire et manipuler les fichiers texte en utilisant Bash. Avec un peu de pratique, tu pourras rapidement devenir un expert en la matière.

## Plongée en profondeur

Maintenant que tu sais comment lire un fichier texte en utilisant Bash, tu peux également apprendre à modifier et à créer des fichiers texte en utilisant des commandes similaires telles que "echo" et "sed". Tu peux également utiliser la redirection pour écrire le résultat de ta commande dans un nouveau fichier texte.

Tu peux également utiliser Bash pour traiter des fichiers CSV (valeurs séparées par des virgules) en utilisant la commande "awk". Cela peut être utile si tu as besoin de manipuler de grandes quantités de données.

Alors, n'hésite pas à explorer davantage les options de Bash pour manipuler des fichiers de texte et gagner du temps dans tes tâches informatiques au quotidien.

## Voir aussi

- [Documentation officielle de Bash](https://www.gnu.org/software/bash/)
- [Guide sur la manipulation de fichiers en utilisant Bash](https://tldp.org/LDP/abs/html/intandnonint.html#DEALINGOUTOFTHEFILE)
- [Tutoriel interactif sur l'utilisation de Bash pour lire et modifier des fichiers](https://www.learnshell.org/en/Read_Commands)