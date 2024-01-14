---
title:    "Elixir: Création d'un fichier temporaire"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi créer un fichier temporaire en Elixir ?

La création de fichiers temporaires est une tâche courante en programmation. Cela peut être utile lorsque vous avez besoin de stocker des données de manière temporaire, comme des fichiers téléchargés ou générés dynamiquement. En utilisant Elixir, vous avez accès à des outils simples et efficaces pour créer et gérer ces fichiers temporaires.

## Comment faire ?

Pour créer un fichier temporaire en Elixir, nous pouvons utiliser la fonction `File.tempfile/2` qui prend en paramètres le chemin d'accès au répertoire de destination et le préfixe du nom de fichier.

Voici un exemple de code :

```Elixir
{ :ok, file } = File.tempfile("/chemin/vers/dossier", "prefixe_")
IO.puts(file) # affiche le chemin d'accès complet du fichier temporaire créé
```

Le fichier temporaire sera automatiquement détruit une fois que le processus aura terminé son exécution. Vous pouvez également définir une durée d'expiration pour le fichier en utilisant le paramètre `expire_after` de la fonction.

Il est également possible d'écrire des données dans le fichier temporaire en utilisant la fonction `IO.binwrite/2` :

```Elixir
IO.binwrite(file, "Contenu du fichier temporaire")
```

Le fichier temporaire peut ensuite être lu en utilisant la fonction `IO.binread/1`.

## Plongez plus profondément !

Les fichiers temporaires créés avec Elixir sont gérés par le système de fichiers du système d'exploitation. Cela signifie qu'ils seront automatiquement détruits lors du nettoyage du système. Cependant, si vous souhaitez les détruire manuellement, vous pouvez utiliser la fonction `File.rm/1` en lui passant le chemin d'accès au fichier temporaire.

Il est également important de noter que si le processus créateur du fichier temporaire est terminé de manière inattendue, le fichier ne sera pas automatiquement supprimé. Il est donc préférable de toujours utiliser la fonction `File.rm/1` pour nettoyer les fichiers temporaires que vous avez créés.

## Voir aussi

- Documentation officielle sur la fonction `File.tempfile/2`
- Tutoriel pour créer et gérer des fichiers temporaires en Elixir : [lien vers le tutoriel en français](https://lyonsinayion.github.io/2019/11/12/file-drop.html)