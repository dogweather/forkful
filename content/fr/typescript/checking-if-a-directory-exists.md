---
title:                "Vérifier si un répertoire existe"
html_title:           "Lua: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce et pourquoi ?

Vérifier l'existence d'un répertoire est un contrôle auquel les programmeurs recourent pour s'assurer qu'un chemin de répertoire spécifique est présent dans le système de fichiers avant de poursuivre les opérations telles que la lecture ou l'écriture. C'est crucial pour éviter les interruptions d'exécution ou les erreurs de lecture/écriture.

## Comment faire :

Voici un code simple en TypeScript qui aide à comprendre comment vérifier l'existence d'un répertoire.

```TypeScript
import * as fs from 'fs';

function isDirectoryExist(path: string): boolean {
  return fs.existsSync(path);
}

console.log(isDirectoryExist('./your_directory_path'));
```

Après avoir exécuté ce code, vous obtiendrez `true` si le répertoire spécifié existe, sinon `false`.

## Deep Dive

Historiquement, la fonction `existsSync()`, que nous avons utilisée ci-dessus, a été mise en dépréciation dans Node.js v0.12.0 et retirée dans Node.js v1.0. Cependant, en raison de son utilisation élevée, elle a été réintroduite dans Node.js v4.0.0.

Une autre méthode pour vérifier l'existence d'un répertoire est d'utiliser `fs.statSync()`, qui fournira plus d'informations sur le chemin demandé.

```TypeScript
import * as fs from 'fs';

function isDirectoryExist(path: string): boolean {
  try {
    return fs.statSync(path).isDirectory();
  } catch (error) {
    return false;
  }
}

console.log(isDirectoryExist('./your_directory_path'));
```
Cette version peut retourner des informations détaillées sur le répertoire, mais peut également déclencher une exception si le chemin n'existe pas. C'est pourquoi nous l'avons placée dans une instruction `try/catch`.

## Voir aussi

1. Documentation de Node.js fs.existsSync(): [fs.existsSync()](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
2. Documentation de Node.js fs.statSync(): [fs.statSync()](https://nodejs.org/api/fs.html#fs_fs_statsync_path_options)
3. Plus d'informations sur la gestion d'exceptions en TypeScript : [TypeScript Deep Dive: Error Handling](https://basarat.gitbook.io/typescript/postmessage/error-handling)
4. Documentation officielle de TypeScript : [TypeScript Doc](https://www.typescriptlang.org/)