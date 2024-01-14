---
title:                "Bash: Recherche et remplacement de texte"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes dans la programmation Bash. Cela permet de modifier rapidement du texte dans un fichier ou un script, ce qui peut faire gagner du temps et éviter les erreurs. Apprenez comment effectuer cette tâche en suivant les étapes ci-dessous.

## Comment faire

La syntaxe de base pour rechercher et remplacer du texte dans Bash est la suivante :

```bash
sed 's/mot_a_remplacer/nouveau_mot/g' fichier.txt
```

Ici, "sed" est la commande pour stream editor, "s" indique que nous voulons effectuer un remplacement, "mot_a_remplacer" est le mot que nous voulons remplacer, "nouveau_mot" est le mot que nous voulons utiliser à la place, et "fichier.txt" est le nom du fichier dans lequel nous voulons effectuer la modification.

Par exemple, si le fichier "exemple.txt" contient le texte "Bonjour monde", nous pouvons utiliser la commande suivante pour le modifier :

```bash
sed 's/Bonjour/Hello/g' exemple.txt
```

Cela remplacera "Bonjour" par "Hello" et produira le texte "Hello monde" dans le fichier.

Nous pouvons également utiliser des expressions régulières pour rechercher et remplacer du texte. Par exemple, si nous voulons remplacer toutes les occurrences de nombres par le mot "numéro" dans un fichier, nous pouvons utiliser la commande suivante :

```bash
sed 's/[0-9]/numéro/g' fichier.txt
```

Cela remplacera tous les chiffres par le mot "numéro".

## Plongée en profondeur

Vous pouvez également utiliser des options pour modifier le comportement de la commande "sed". Voici quelques-unes des options les plus courantes :

- "i" : insérer du texte avant une ligne correspondante
- "c" : remplacer la ligne entière par un nouveau texte
- "d" : supprimer la ligne correspondante
- "p" : imprimer la ligne correspondante
- "g" : remplacer toutes les occurrences de la ligne correspondante
- "n" : lire la ligne suivante

Par exemple, si nous voulons remplacer une ligne entière par un nouveau texte dans un fichier, nous pouvons utiliser la commande suivante :

```bash
sed '3c/Nouveau texte/' fichier.txt
```

Cela remplacera la troisième ligne dans le fichier par "Nouveau texte".

## Voir aussi

Pour en savoir plus sur la commande "sed" et ses nombreuses options, consultez ces ressources :

- [Document officiel pour la commande "sed"](https://www.gnu.org/software/sed/manual/sed.html)
- [Guide pratique pour utiliser la commande "sed"](https://www.digitalocean.com/community/tutorials/the-basics-of-using-the-sed-stream-editor-to-manipulate-text-in-linux)
- [Tutoriel vidéo sur la commande "sed"](https://www.youtube.com/watch?v=kEltwTZBI8U)