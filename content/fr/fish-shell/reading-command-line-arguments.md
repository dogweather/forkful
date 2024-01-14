---
title:    "Fish Shell: Lecture des arguments en ligne de commande"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes un programmeur ou un administrateur système, vous savez probablement déjà à quel point le terminal peut être puissant. Mais saviez-vous qu'il est possible de passer des arguments en ligne de commande avec Fish Shell ? Si vous souhaitez automatiser des tâches ou simplement gagner du temps en utilisant la ligne de commande, lire des arguments en est une compétence essentielle.

## Comment faire 

La syntaxe pour lire des arguments en ligne de commande avec Fish Shell est simple : 

```
Fish Shell <nom du script> <arguments> 
```

Par exemple, si vous avez un script nommé "mon_script.fish" et que vous souhaitez lui passer les arguments "foo" et "bar", vous pouvez taper : 

```
Fish Shell mon_script.fish foo bar 
```

Dans votre script, vous pouvez accéder aux arguments en utilisant les variables spéciales $argv et $argc. Par exemple, $argv[1] sera égal à "foo" et $argc sera égal à 2.

## Plongée en profondeur 

Maintenant que vous savez comment lire des arguments en ligne de commande avec Fish Shell, voici quelques astuces supplémentaires. Vous pouvez accéder à l'ensemble des arguments en utilisant $argv ou $argc, mais vous pouvez également utiliser des index négatifs pour obtenir les arguments en partant de la fin. Par exemple, $argv[-1] sera égal à "bar" dans notre exemple précédent.

De plus, si vous devez traiter des arguments avec des espaces, vous pouvez les entourer de guillemets doubles pour les séparer en un seul argument. Par exemple, vous pouvez passer "bar baz" comme un seul argument en utilisant "bar baz" dans la ligne de commande.

## Voir aussi 

Pour en savoir plus sur la lecture des arguments en ligne de commande avec Fish Shell, voici quelques liens utiles : 

- Documentation officielle de Fish Shell sur la lecture des arguments : https://fishshell.com/docs/current/cmds/exec.html#reading-arguments
- Tutoriel vidéo sur la lecture des arguments en ligne de commande avec Fish Shell : https://www.youtube.com/watch?v=po_ec6KQJiw
- Exemples de scripts utilisant des arguments en ligne de commande avec Fish Shell : https://github.com/fish-shell/fish-shell/tree/master/doc_src/commands

Maintenant que vous maîtrisez la lecture des arguments en ligne de commande avec Fish Shell, vous pourrez automatiser des tâches plus rapidement et plus efficacement. Bon codage !