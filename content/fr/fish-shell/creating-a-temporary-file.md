---
title:    "Fish Shell: Création d'un fichier temporaire"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

Pourquoi: Pourquoi créer un fichier temporaire avec Fish Shell ?
Il existe de nombreuses raisons pour lesquelles quelqu'un pourrait vouloir créer un fichier temporaire lors de la programmation en Fish Shell. Certaines raisons incluent l'utilisation de mémoires tampons, la gestion de fichiers temporaires pour des tâches spécifiques, ou simplement pour avoir un fichier temporaire pour stocker des données temporaires sans risquer de les perdre.

Comment faire: Exemples de code et résultats à l'aide de "```Fish Shell ... ```" blocs de code.
Créer un fichier temporaire avec Fish Shell est très simple. Tout d'abord, vous devez utiliser la commande ``mktemp`` pour créer un fichier temporaire vide, comme ceci :
```Fish Shell
set temp_file (mktemp)
```
Ensuite, vous pouvez utiliser ce fichier pour stocker des données temporaires, par exemple :
```Fish Shell
echo "Données temporaire" > $temp_file
```
Vous pouvez également utiliser des variables pour stocker ces données, comme ceci :
```Fish Shell
set var "Données temporaire"
echo $var > $temp_file
```
Une fois que vous avez fini d'utiliser le fichier temporaire, vous pouvez le supprimer avec la commande ``rm`` :
```Fish Shell
rm $temp_file
```

Plongeons plus profondément: Informations plus détaillées sur la création de fichiers temporaires.
Lorsque vous créez un fichier temporaire avec Fish Shell, il est important de spécifier un modèle pour le nom du fichier. Cela garantit que le fichier sera unique et évite les conflits avec d'autres fichiers. Par défaut, ce modèle est ``/tmp/tmp.YYYYMMDD.HHMMSS.XXXXXX``, mais vous pouvez le personnaliser en utilisant l'option ``-p`` avec la commande ``mktemp``, comme ceci :
```Fish Shell
set temp_file (mktemp -p "/chemin/vers/le/dossier/temporaire/tmp.XXXX")
```
Vous pouvez également spécifier une extension pour le fichier temporaire en utilisant l'option ``--suffix``, comme ceci :
```Fish Shell
set temp_file (mktemp --suffix ".txt")
```
En plus de stocker des données temporaires, vous pouvez également utiliser un fichier temporaire comme un verrou pour empêcher l'accès à certaines ressources pendant qu'il est utilisé. Vous pouvez également utiliser un fichier temporaire pour stocker des données sensibles, en utilisant l'option ``-t`` avec la commande ``mktemp``, qui supprime le fichier temporaire dès qu'il n'est plus utilisé.

Voir aussi: Pour plus d'informations sur la création de fichiers temporaires avec Fish Shell, vous pouvez consulter les liens suivants :
- La documentation officielle de Fish Shell : https://fishshell.com/docs/current/cmds/mktemp.html
- Un tutoriel sur la création de fichiers temporaires en utilisant différentes options : https://spin.atomicobject.com/2013/09/30/temporary-files-in-fish-shell/
- Une discussion sur Stack Overflow sur l'utilité et les avantages de créer des fichiers temporaires : https://stackoverflow.com/questions/909388/why-use-a-temporary-file-instead-of-a-string