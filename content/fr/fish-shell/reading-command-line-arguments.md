---
title:    "Fish Shell: Lecture des arguments de ligne de commande"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Vous avez peut-être entendu parler de programmation en ligne de commande et de l'utilisation de lignes de commande pour exécuter des tâches sur votre ordinateur. Mais saviez-vous que le shell Fish offre une solution plus conviviale pour travailler avec des arguments de ligne de commande ? Dans cet article, nous allons plonger dans le monde de la programmation en ligne de commande avec Fish et découvrir pourquoi vous devriez envisager d'utiliser ses fonctionnalités.

## Comment faire

Fish Shell facilite la lecture des arguments de ligne de commande grâce à sa syntaxe simple et intuitive. Voici un exemple de commande qui utilise un argument de ligne de commande :

```Fish Shell
cp monfichier.txt ~/Documents/
```

Dans cet exemple, `monfichier.txt` est un argument de la commande `cp`, qui copie le fichier dans le dossier `Documents` de votre utilisateur. Vous pouvez également utiliser des arguments pour définir des options, comme dans cet exemple :

```Fish Shell
ls -l
```

Ici, l'argument `-l` est utilisé pour demander à la commande `ls` d'afficher les résultats en format long.

## Plongée en profondeur

Outre les arguments simples, Fish Shell offre également la possibilité de lire des arguments combinés. Par exemple, vous pouvez utiliser un seul argument pour exécuter des commandes successives :

```Fish Shell
echo Cela est du texte || echo Cela n'est pas du texte
```

Dans cet exemple, la commande `echo` sera exécutée deux fois, mais seules les instructions après l'opérateur `||` seront imprimées à l'écran.

Vous pouvez également utiliser des arguments dans des scripts pour automatiser des tâches récurrentes. Par exemple, vous pourriez créer un script qui prend en argument le nom d'un fichier, puis le copie dans un dossier spécifique. Cette fonctionnalité peut être très utile pour gagner du temps et éviter de saisir une commande complète à chaque fois que vous avez besoin de faire la même tâche.

## Voir aussi

Si vous souhaitez en savoir plus sur la programmation en ligne de commande avec Fish Shell, voici quelques ressources supplémentaires que vous pourriez trouver utiles :

- [La documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Un tutoriel sur les bases de la programmation avec Fish Shell](https://www.digitalocean.com/community/tutorials/an-introduction-to-the-fish-shell)
- [Une liste de commandes utiles pour Fish Shell](https://github.com/jorgebucaran/awesome-fish)