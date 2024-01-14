---
title:                "Javascript: Création d'un fichier temporaire"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi créer des fichiers temporaires en Javascript?

Si vous êtes un développeur Javascript, vous avez probablement déjà rencontré cette situation: vous avez besoin de stocker temporairement des données ou des résultats de votre code, mais vous ne voulez pas créer de fichiers permanents.

Cela peut être pour des raisons de confidentialité, de performance ou simplement pour éviter l'encombrement de votre système de fichiers. Dans ces cas-là, utiliser des fichiers temporaires peut être une solution efficace.

## Comment créer un fichier temporaire en Javascript?

Heureusement, Javascript offre une solution simple et pratique pour créer des fichiers temporaires : l'API File System (FS). Voici un exemple de code:

```Javascript
const fs = require('fs');

// Créer un fichier temporaire avec le préfixe "temp" et l'extension ".txt"
fs.mkdtemp('temp', (err, folder) => {
    if (err) throw err;

    // Écrire des données dans le fichier temporaire
    fs.writeFile(`${folder}/temp.txt`, 'Ceci est un fichier temporaire', (err) => {
        if (err) throw err;

        // Lire les données du fichier temporaire
        fs.readFile(`${folder}/temp.txt`, (err, data) => {
            if (err) throw err;
        
            // Afficher les données
            console.log(data.toString());
        });
    });
});
```

Sortie:

```
Ceci est un fichier temporaire
```

Comme vous pouvez le voir, nous utilisons la méthode `mkdtemp()` pour créer un dossier temporaire avec un préfixe spécifié (`temp` dans cet exemple). Ensuite, nous écrivons des données dans le fichier temporaire en utilisant `writeFile()` et nous les lisons avec `readFile()`.

## Plus d'informations sur la création de fichiers temporaires

L'API FS offre différentes options pour la création et la gestion de fichiers temporaires. Vous pouvez spécifier le dossier où créer le fichier temporaire, le préfixe et l'extension du fichier, ainsi que d'autres paramètres. Vous pouvez également supprimer le fichier temporaire une fois que vous n'en avez plus besoin en utilisant la méthode `unlink()`.

Pour plus d'informations sur ces options, vous pouvez consulter la documentation officielle de l'API FS : https://nodejs.org/api/fs.html#fs_file_system_flags

## Voir aussi

- Tutoriel sur l'API FS en node.js (en français) : https://samypesse.fr/nodejs/fs/
- Article sur la création de fichiers temporaires en Javascript : https://medium.com/@dreyacosta/creating-temporary-files-in-javascript-5331d8d3551f