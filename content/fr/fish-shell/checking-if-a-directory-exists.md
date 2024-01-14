---
title:                "Fish Shell: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Avant de commencer à utiliser Fish Shell, il est important de comprendre les bases de la programmation et des commandes de base. Une tâche courante en programmation est de vérifier si un répertoire existe ou non. Dans cet article, nous allons explorer comment vous pouvez le faire en utilisant la syntaxe du Shell Fish.

## Comment faire

Tout d'abord, il faut savoir que pour vérifier si un répertoire existe, nous allons utiliser la fonction ```fish_is_dir```. Cette fonction renvoie ```1``` si le répertoire existe, sinon elle renvoie ```0```.

Voici un exemple de code pour vérifier si le répertoire "Documents" existe dans votre dossier personnel :

```Fish Shell
if fish_is_dir ~/Documents
  echo "Le répertoire 'Documents' existe."
else
  echo "Le répertoire 'Documents' n'existe pas."
end
```

Lorsque vous exécutez ce script, il va afficher "Le répertoire 'Documents' existe." si le répertoire existe, sinon il va afficher "Le répertoire 'Documents' n'existe pas."

Vous pouvez également utiliser cette fonction dans une commande conditionnelle, par exemple pour supprimer un répertoire seulement s'il existe :

```Fish Shell
if fish_is_dir ~/Documents
  rm -r ~/Documents
else
  echo "Le répertoire 'Documents' n'existe pas."
end
```

Dans cet exemple, si le répertoire "Documents" existe, alors il sera supprimé. Sinon, un message d'erreur sera affiché.

## Deep Dive

Maintenant que vous comprenez comment utiliser la fonction ```fish_is_dir```, il est important de noter qu'elle est différente de la commande système ```test -d```. En effet, cette dernière va renvoyer un code d'erreur si le répertoire n'existe pas, tandis que ```fish_is_dir``` renvoie simplement ```0```.

Vous pouvez également utiliser la fonction ```fish_is_dir``` pour vérifier si un chemin est un répertoire ou un fichier :

```Fish Shell
if fish_is_dir ~/Documents
  echo "~/Documents est un répertoire."
else
  echo "~/Documents est un fichier."
end
```

Cela peut être utile si vous avez besoin de savoir quel type de chemin vous avez en entrée pour votre script.

## Voir aussi

Pour plus d'informations sur les commandes et fonctions de Fish Shell, voici quelques liens utiles :

- [Documentation officielle Fish Shell](https://fishshell.com/docs/current/)
- [Guide de référence pour la syntaxe du Shell Fish](https://fishshell.com/docs/current/tutorial.html#syntax)
- [Liste complète des fonctions du Shell Fish](https://fishshell.com/docs/current/cmds/index.html)
- [Guide pour débutants en programmation avec Fish Shell](https://fishshell.com/docs/current/tutorial.html#tutorial-basics)