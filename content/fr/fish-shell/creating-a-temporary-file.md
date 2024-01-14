---
title:    "Fish Shell: Création d'un fichier temporaire"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Pourquoi

Nous avons tous été confrontés à la création de fichiers temporaires lors de la programmation. Mais pourquoi est-ce important? La réponse courte est que les fichiers temporaires sont des outils utiles pour stocker des données temporaires ou pour effectuer des opérations qui nécessitent un fichier en tant qu'entrée.

## Comment faire

Pour créer un fichier temporaire en utilisant Fish Shell, vous pouvez utiliser la commande `mktemp`. Voici un exemple de code et de la sortie correspondante :

```
Fish Shell code : 
·```
mktemp temp.txt
·```

Output :
```
temp.txt14562
```

Il est important de noter que le nombre après le nom de fichier est un identifiant unique généré par le système pour garantir que le fichier temporaire créé est unique et n'interfère pas avec d'autres fichiers.

## Plongeons plus loin

Bien qu'il soit possible de créer un fichier temporaire avec une simple commande, il est important de comprendre comment cela fonctionne en termes de code. La commande `mktemp` utilise en fait le système de fichiers pour créer un fichier vide avec un nom unique, puis renvoie ce nom à la commande.

Il est également possible de spécifier le préfixe pour le nom du fichier temporaire en utilisant le drapeau `-p`. Par exemple :

```
Fish Shell code :
·```
mktemp -p foo temp.txt
·```

Output :
```
footemp.txt51234
```

Cela créera un fichier temporaire nommé "footemp.txt" dans le répertoire "foo" avec un identifiant unique généré par le système.

## Voir aussi

- [La documentation officielle de Fish Shell sur `mktemp`](https://fishshell.com/docs/current/cmds/mktemp.html)
- [Un guide pratique pour créer et utiliser des fichiers temporaires en programmation](https://www.linuxjournal.com/content/creating-and-using-temporary-files-programming)
- [Un tutoriel sur l'utilisation de Fish Shell pour les débutants](https://figata.cc/blog/fish-shell-for-beginners/)