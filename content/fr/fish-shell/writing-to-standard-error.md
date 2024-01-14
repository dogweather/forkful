---
title:    "Fish Shell: Écrire vers l'erreur standard"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi

Ecrire vers la sortie d'erreur standard est une pratique courante dans la programmation en Fish Shell. Cela permet de contrôler et de gérer les erreurs de manière efficace, ce qui peut améliorer la fiabilité et la qualité de votre code.

# Comment faire

Pour écrire vers la sortie d'erreur standard en Fish Shell, il suffit d'utiliser la commande `echo` suivie du symbole `>` et du numéro de sortie `2` pour indiquer l'erreur. Par exemple :

```Fish Shell

echo "Il y a eu une erreur" >2

```

Cela écrira le message "Il y a eu une erreur" vers la sortie d'erreur standard. Vous pouvez également ajouter des variables ou des commandes à votre message d'erreur, comme ceci :

```Fish Shell

echo "L'erreur suivante s'est produite : $erreur" >2

```

Dans cet exemple, la variable `$erreur` sera remplacée par la valeur correspondante lors de l'exécution du code. Vous pouvez également utiliser `>>` pour ajouter un message à la sortie d'erreur standard sans l'écraser.

Pour afficher le contenu de la sortie d'erreur standard, vous pouvez utiliser la commande `cat` suivie du fichier spécial `/dev/stderr`. Par exemple :

```Fish Shell

cat /dev/stderr

```

Cela affichera tout le contenu de la sortie d'erreur standard à l'écran.

# Plongée en profondeur

L'utilisation de la sortie d'erreur standard permet également de rediriger les erreurs vers un fichier ou de les utiliser pour les exploiter ultérieurement. Par exemple, vous pouvez utiliser `2> fichier.txt` pour écrire les erreurs vers un fichier spécifique.

De plus, en utilisant les commandes `2>&1` ou `|&`, vous pouvez rediriger à la fois la sortie standard et la sortie d'erreur standard vers le même endroit. Cela peut être utile pour simplifier le traitement des erreurs et des résultats.

# Voir aussi

- [Documentation Fish Shell](https://fishshell.com/docs/current/index.html)
- [Guide de démarrage rapide de Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Commandes de redirection en Fish Shell](https://fishshell.com/docs/current/tutorial.html#redirecting-the-input-output-and-errors)