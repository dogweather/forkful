---
title:                "TypeScript: La lecture d'un fichier texte"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur TypeScript, il est important de comprendre comment lire des fichiers texte dans votre code. Que vous ayez besoin de traiter des données stockées dans un fichier texte ou de lire un fichier de configuration pour votre application, savoir comment le faire correctement peut vous faire gagner beaucoup de temps et vous éviter des erreurs.

## Comment faire

Lire un fichier texte en TypeScript est assez simple. Tout d'abord, vous devez installer le package "fs" à l'aide du gestionnaire de paquets npm. Ensuite, vous pouvez utiliser la méthode "readFileSync" pour lire le contenu du fichier. Voici un exemple de code :

```TypeScript
import * as fs from "fs";

// Lire le contenu du fichier texte
const content = fs.readFileSync("sample.txt", "utf-8");

// Afficher le contenu dans la console
console.log(content);
```

En utilisant la méthode "readFileSync" avec le paramètre "utf-8", nous nous assurons que le contenu du fichier est lu en tant que chaîne de caractères et non en tant que données binaires.

Un autre moyen de lire un fichier texte est d'utiliser la méthode "readFile", qui prend en charge les fonctions de rappel asynchrones. Cela peut être utile si vous devez exécuter d'autres tâches pendant que le fichier est en cours de lecture. Voici un exemple de code avec la méthode "readFile" :

```TypeScript
import * as fs from "fs";

// Lire le contenu du fichier texte en utilisant un rappel
fs.readFile("sample.txt", "utf-8", (err, data) => {
    if (err) {
        console.log(err);
    } else {
        // Afficher le contenu dans la console
        console.log(data);
    }
});
```

## Plongée profonde

En plus de lire le contenu d'un fichier texte, il est également important de comprendre comment gérer les erreurs potentielles lors de la lecture du fichier. L'utilisation de blocs "try-catch" est une bonne pratique pour s'assurer que votre code gère correctement toutes les erreurs possibles.

De plus, si vous avez besoin de traiter le contenu du fichier ligne par ligne, vous pouvez utiliser la méthode "readline" du package "readline". Cela vous permet de lire le contenu du fichier de manière asynchrone et de gérer chaque ligne individuellement. Voici un exemple de code :

```TypeScript
import * as fs from "fs";
import * as readline from "readline";

// Créer une interface de lecture
const rl = readline.createInterface({
    input: fs.createReadStream("sample.txt"),
    crlfDelay: Infinity
});

// Parcourir chaque ligne du fichier et les afficher dans la console
rl.on("line", (line) => {
    console.log(line);
});
```

## Voir aussi

Vous pouvez en apprendre davantage sur la lecture de fichiers textes en TypeScript en consultant les ressources suivantes :

- Documentation officielle de Node.js sur la lecture de fichiers : https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options
- Tutoriel sur la lecture et l'écriture de fichiers en TypeScript : https://www.digitalocean.com/community/tutorials/reading-and-writing-files-in-node-js-using-the-fs-module
- Exemples de code pour lire des fichiers en TypeScript : https://github.com/TypeScript-Node-Starter/TypeScript-Node-Starter/tree/master/src/api/routes/file

Maintenant, vous avez toutes les connaissances nécessaires pour lire des fichiers texte en TypeScript. N'hésitez pas à explorer davantage et à expérimenter avec différents types de fichiers et de méthodes pour trouver ce qui fonctionne le mieux pour votre code. Bonne lecture !