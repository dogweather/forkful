---
title:                "Verifica dell'esistenza di una directory"
html_title:           "TypeScript: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perche

Ci sono molti motivi per cui si potrebbe voler controllare se una directory esiste in JavaScript, come ad esempio verificare l'esistenza di un file prima di scrivere sui dati o controllare se una cartella è stata correttamente creata. In ogni caso, è una buona pratica essere consapevoli della presenza di una directory prima di utilizzarla nel codice.

## Come fare

Controllare se una directory esiste in TypeScript è un processo semplice e diretto. Basta utilizzare la funzione "existsSync" del modulo "fs" di Node.js e passare il percorso completo della directory come parametro. Se la directory esiste, la funzione restituirà "true", altrimenti restituirà "false".

```TypeScript
import { existsSync } from 'fs';

if (existsSync('/percorso/directory')) {
  console.log("La directory esiste!");
} else {
  console.log("La directory non esiste!");
}
```

Output:

```
La directory esiste!
```

## Approfondimento

La funzione "existsSync" utilizza il sistema operativo per controllare se una directory esiste, quindi è necessario prestare attenzione ai diversi comportamenti in base alla piattaforma. Ad esempio, su Windows, la funzione può accettare sia i percorsi con barre oblique che con barre rovesciate, mentre su Linux solo i percorsi con barre oblique saranno accettati.

Inoltre, è possibile utilizzare la funzione "statSync" del modulo "fs" per ottenere informazioni più dettagliate sulla directory, come ad esempio la data di creazione o l'ultima data di modifica. Ci sono anche altre librerie open-source disponibili per controllare se una directory esiste, come "fs-exists-sync" e "fs-extra".

## Vedi Anche

- Documentazione ufficiale di Node.js su "fs.existsSync": https://nodejs.org/api/fs.html#fs_fs_existssync_path
- Libreria "fs-exists-sync": https://www.npmjs.com/package/fs-exists-sync
- Libreria "fs-extra": https://www.npmjs.com/package/fs-extra