---
title:                "TypeScript: Vérification de l'existence d'un répertoire"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est important de vérifier l'existence d'un répertoire lors de la programmation TypeScript car cela permet de s'assurer que votre code peut traiter toutes les situations possibles, y compris lorsque le répertoire cible n'existe pas.

## Comment faire

La vérification de l'existence d'un répertoire peut être facilement réalisée en utilisant les méthodes natives de TypeScript. Voici un exemple de code pour vérifier si un répertoire existe :

```TypeScript
function checkDirectory(directoryName: string) {
    if(fs.existsSync(directoryName)) {
        console.log(`Le répertoire ${directoryName} existe.`);
    } else {
        console.log(`Le répertoire ${directoryName} n'existe pas.`);
    }
}

checkDirectory("monRepertoire");
```

Lorsque vous exécutez ce code, la sortie sera soit "Le répertoire monRepertoire existe." ou "Le répertoire monRepertoire n'existe pas." en fonction de la présence ou de l'absence du répertoire spécifié.

## Plongez plus en profondeur

Maintenant que vous savez comment vérifier si un répertoire existe, vous pouvez aller plus loin en manipulant le répertoire lui-même. Vous pouvez utiliser la méthode `mkdir()` pour créer un nouveau répertoire ou `rmdir()` pour le supprimer. Vous pouvez également utiliser `readdir()` pour afficher le contenu d'un répertoire existant.

Cependant, n'oubliez pas de toujours vérifier l'existence d'un répertoire avant de tenter de le manipuler pour éviter les erreurs dans votre code.

## Voir aussi

- [Documentation officielle de TypeScript](https://www.typescriptlang.org/docs/)
- [Guide de démarrage avec TypeScript](https://www.tutorialspoint.com/typescript/typescript_quick_guide.htm)
- [Gestion des fichiers et répertoires avec TypeScript](https://morioh.com/p/a0af96b6c830)