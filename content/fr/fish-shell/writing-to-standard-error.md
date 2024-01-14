---
title:                "Fish Shell: Écriture vers l'erreur standard"
simple_title:         "Écriture vers l'erreur standard"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi 

L'écriture sur la sortie d'erreur standard, ou "standard error" en anglais, est une pratique importante dans la programmation en Fish Shell. Cela permet aux programmeurs de signaler des erreurs ou des informations importantes aux utilisateurs lors de l'exécution d'un programme.

## Comment faire 

Pour écrire sur la sortie d'erreur standard en utilisant Fish Shell, vous pouvez utiliser la commande `echo` suivie de l'option `-e` pour activer l'interprétation des caractères spéciaux et l'option `-s` pour écrire sur la sortie d'erreur standard. Par exemple : 

```
echo -e "Une erreur est survenue ! \n" >&2
```

Cela écrira le message "Une erreur est survenue !" sur la sortie d'erreur standard, qui sera affiché à l'utilisateur.

## Plongée en profondeur 

La sortie d'erreur standard est un flux réservé pour l'affichage des messages d'erreur, de débogage et d'informations importantes. Elle est séparée de la sortie standard pour permettre une gestion plus précise des données.

Dans Fish Shell, vous pouvez utiliser le symbole `2>` pour rediriger la sortie d'erreur standard vers un fichier ou un autre flux. Vous pouvez également utiliser `&>` pour rediriger à la fois la sortie standard et la sortie d'erreur standard vers le même endroit.

Par exemple, pour rediriger la sortie d'erreur standard vers un fichier nommé "erreurs.log", vous pouvez utiliser la commande suivante : 

```
ls -l /dossier/inexistant 2> erreurs.log
```

Cela créera un fichier contenant les informations d'erreur provenant de la commande `ls`. Vous pouvez ensuite utiliser la commande `cat` pour afficher le contenu du fichier d'erreurs.

## Voir aussi 

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutoriel sur la redirection de la sortie en Fish Shell](https://dev.to/kojinkai/how-to-redirect-the-output-75c)
- [Forum Fish Shell](https://github.com/fish-shell/fish-shell/discussions)