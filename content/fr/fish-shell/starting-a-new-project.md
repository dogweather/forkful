---
title:    "Fish Shell: Commencer un nouveau projet"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur ou un amateur de la ligne de commande, vous savez probablement l'importance de trouver le bon shell pour travailler efficacement. Le Fish Shell est un bon choix pour de nombreuses raisons, notamment sa syntaxe simple et intuitive, sa saisie semi-automatique et ses fonctionnalités de personnalisation. Dans cet article, je vais vous montrer comment démarrer un nouveau projet avec Fish Shell et vous plonger dans ses fonctionnalités plus avancées.

## Comment faire

Pour commencer un nouveau projet avec Fish Shell, vous devrez d'abord installer Fish Shell sur votre système. Vous pouvez le faire en utilisant votre gestionnaire de paquets ou en téléchargeant les binaires à partir du site officiel. Une fois installé, vous pouvez suivre ces étapes pour créer un nouveau projet.

1. Tout d'abord, créez un dossier pour votre projet en utilisant la commande `mkdir` : 

```
mkdir mon_projet
```

2. Ensuite, déplacez-vous dans ce dossier à l'aide de la commande `cd` :

```
cd mon_projet
```

3. Maintenant, vous pouvez initialiser un nouveau référentiel Git à l'aide de la commande `git init` :

```
git init
```

4. Pour commencer à coder, vous pouvez utiliser n'importe quel éditeur de texte ou utiliser l'éditeur de Fish Shell en tapant simplement `fish` :

```
fish
```

5. Commencez à écrire votre code et enregistrez-le sous le nom souhaité, par exemple `main.sh`.

6. Maintenant, vous pouvez exécuter votre code en utilisant la commande `source` :

```
source main.sh
```

7. Vous verrez la sortie de votre code directement dans votre terminal.

## Plongée en profondeur

Fish Shell a de nombreuses fonctionnalités avancées qui peuvent vous faciliter la vie lors de la création de nouveaux projets.

- Saisie semi-automatique : Fish Shell offre une saisie semi-automatique avancée qui peut vous aider à taper plus rapidement en suggérant des commandes et des chemins de fichier.

- Complétion des commandes : en appuyant sur la touche `tab`, Fish Shell complète automatiquement les commandes, les options et les chemins de fichier.

- Personnalisation : vous pouvez personnaliser votre Fish Shell en ajoutant des plugins et en changeant le thème pour répondre à vos besoins.

- Historique des commandes : Fish Shell garde une trace de vos commandes précédentes pour que vous puissiez facilement y accéder en utilisant les touches fléchées.

## Voir aussi

- [Guide de démarrage rapide de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Plugins Fish Shell](https://github.com/oh-my-fish/oh-my-fish)