---
title:                "Vérifier si un répertoire existe"
html_title:           "Javascript: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Vérifier si un répertoire existe en JavaScript

## Qu'est-ce que c'est et pourquoi ?

Vérifier si un répertoire existe est une technique de base en programmation informatique. C'est un contrôle utile qui prévient les erreurs avant qu'elles ne se produisent, par exemple lorsqu'une application essaie d'écrire dans un répertoire qui n'existe pas.

## Comment faire :

Voici comment vérifier si un répertoire existe en utilisant Node.js :

```Javascript
const fs = require('fs');

fs.access('/chemin/vers/ton/dossier', error => {
  if (!error) {
    console.log('Le répertoire existe.');
  } else {
    console.log('Le répertoire n\'existe pas.');
  }
});
```

Dans cet exemple, le module 'fs' de Node.js est requis pour accéder au système de fichiers. La méthode 'access' vérifie l'existence du répertoire. Si le répertoire existe, la fonction de rappel ne reçoit pas d'erreur. Si le répertoire n'existe pas, une erreur sera renvoyée.

## Plongée en profondeur

Historiquement, dans des versions plus anciennes de Node.js, la méthode 'exists' était utilisée pour vérifier l'existence de fichiers et de répertoires. Cependant, elle a été dépréciée en faveur de la méthode 'access'.

Une autre méthode pour vérifier l'existence d'un répertoire est d'essayer d'ouvrir ou de lire son contenu avec 'fs.readdir' ou 'fs.open'. Si ces méthodes ne renvoient pas d'erreur, cela signifie que le répertoire existe.

Toutes ces méthodes sont asynchrones, ce qui signifie qu'elles ne bloquent pas le fil d'exécution principal. C'est une bonne pratique en JavaScript, car cela rend les applications plus performantes.

## Voir aussi

Pour plus d'informations et de détails techniques, consultez la documentation officielle Node.js ici : https://nodejs.org/api/fs.html#fs_fs_access_path_mode_callback