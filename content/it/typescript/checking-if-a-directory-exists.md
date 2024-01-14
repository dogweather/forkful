---
title:                "TypeScript: Verifica dell'esistenza di una directory"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Spesso, durante la scrittura di un programma, ci troviamo nella necessità di verificare se una determinata directory esiste o meno. Questo può essere utile per condurre controlli sulla struttura del nostro file system o per gestire eventuali conflitti di nomi.

## Come Fare

Per verificare se una directory esiste in TypeScript, possiamo utilizzare il metodo `fs.existsSync()` della libreria `fs`. Questo metodo accetta come parametro il percorso della directory che vogliamo controllare e restituirà `true` se la directory esiste, `false` altrimenti.

```TypeScript
import * as fs from 'fs';

if (fs.existsSync("./directory")) {
  console.log("La directory esiste!");
} else {
  console.log("La directory non esiste.");
}
```

Output:
```
La directory esiste!
```

Possiamo anche utilizzare questo metodo per eseguire controlli prima di creare una nuova directory o per gestire errori in caso la directory non esista.

## Approfondimento

Per effettuare una verifica più precisa, possiamo utilizzare il metodo `fs.statSync()` che ci permette di ottenere informazioni dettagliate sulla directory. Se la directory non esiste, questo metodo restituirà un errore che possiamo gestire nel nostro codice.

```TypeScript
import * as fs from 'fs';

try {
  const stats = fs.statSync("./directory");
  console.log("Dimensione della directory:", stats.size);
  console.log("Data di creazione:", stats.birthtime);
} catch (err) {
  console.log("La directory non esiste o si è verificato un errore.");
}
```

Output:
```
Dimensione della directory: 2389
Data di creazione: 2021-08-10T12:00:00.000Z
```

Inoltre, possiamo anche utilizzare il metodo `fs.lstatSync()` per verificare se una directory è un link simbolico e ottenere le informazioni relative al file a cui è collegato.

## Vedi Anche

- [Documentazione ufficiale fs.existsSync()](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Documentazione ufficiale fs.statSync()](https://nodejs.org/api/fs.html#fs_fs_statsync_path_options)
- [Documentazione ufficiale fs.lstatSync()](https://nodejs.org/api/fs.html#fs_fs_lstatsync_path_options)