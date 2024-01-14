---
title:    "Fish Shell: Lecture d'un fichier texte"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur débutant ou un utilisateur régulier de Fish Shell, vous avez probablement entendu parler de la lecture de fichiers texte. Mais qu'est-ce que cela signifie exactement et pourquoi devriez-vous l'utiliser dans votre code? Lisez la suite pour en savoir plus sur cette fonctionnalité et comment elle peut vous être utile.

## Comment Faire

Pour lire un fichier texte dans Fish Shell, utilisez simplement la commande `cat` suivie du nom du fichier:

```Fish Shell
cat mon_fichier.txt
```

Cela affichera le contenu du fichier sur votre terminal. Si vous voulez rediriger le contenu vers un autre fichier, vous pouvez utiliser l'opérateur de redirection `>`:

```Fish Shell
cat mon_fichier.txt > nouveau_fichier.txt
```

Vous pouvez également ajouter du contenu à un fichier existant en utilisant l'opérateur de redirection `>>`:

```Fish Shell
cat mon_fichier.txt >> autre_fichier.txt
```

En plus de `cat`, il existe d'autres commandes dans Fish Shell que vous pouvez utiliser pour lire des fichiers texte, telles que `head`, `tail`, `more`, `less`, etc. Explorez-les pour découvrir leurs différentes fonctionnalités.

## Plongée Profonde

Lorsque vous lisez un fichier texte dans Fish Shell, il est important de connaître certains détails techniques qui peuvent être utiles pour effectuer des tâches plus complexes. Par exemple, vous pouvez spécifier le nombre de lignes que vous souhaitez afficher en utilisant le paramètre `-n`, ou vous pouvez utiliser l'option `-v` pour afficher également les caractères invisibles. Il existe également d'autres paramètres que vous pouvez utiliser pour personnaliser votre lecture de fichier.

De plus, il est important de noter que les fichiers texte peuvent être lus de différentes manières en fonction du système d'exploitation sur lequel vous utilisez Fish Shell. Vous devriez donc garder cela à l'esprit lorsque vous travaillez avec des fichiers texte.

## Voir Aussi

- [La documentation officielle de Fish Shell sur la lecture de fichiers texte](https://fishshell.com/docs/current/cmds/cat.html)
- [Un guide complet sur l'utilisation de la commande `cat`](https://www.tecmint.com/cat-command-usage-with-examples/)
- [Un tutoriel vidéo sur la lecture de fichiers texte dans Fish Shell](https://www.youtube.com/watch?v=dQgkpjDIbfk)

Maintenant que vous avez appris comment lire des fichiers texte dans Fish Shell, vous pouvez l'utiliser pour développer des scripts plus avancés et automatiser certaines tâches sur votre système. Amusez-vous bien!