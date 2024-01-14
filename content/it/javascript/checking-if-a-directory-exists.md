---
title:                "Javascript: Verifica dell'esistenza di una directory"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché controllare se una directory esiste

Quando si programma in JavaScript, a volte è necessario controllare se una directory specificata esiste o meno. Questa informazione è importante per assicurarsi di poter accedere ai file desiderati e per evitare errori durante l'esecuzione del codice.

## Come fare

Per controllare se una directory esiste in JavaScript, è possibile utilizzare il metodo `existsSync()` del modulo `fs`. Questo metodo restituisce un valore booleano, `true` se la directory esiste e `false` se non esiste.

```Javascript
const fs = require('fs');

// Specifica la directory da controllare
const directory = "path/to/directory";

if (fs.existsSync(directory)) {
  console.log(`${directory} esiste.`);
} else {
  console.log(`${directory} non esiste.`);
}

// Output (si suppone che la directory non esista)
// "path/to/directory non esiste."
```

In questo esempio, stiamo utilizzando il modulo `fs` per controllare la presenza di una directory specifica. Usando il metodo `existsSync()`, siamo in grado di eseguire un controllo condizionale per determinare se la directory esiste o no.

## Approfondimento

Quando si esegue un controllo se una directory esiste in JavaScript, è importante tenere presente alcune cose. In primo luogo, il metodo `existsSync()` funziona sincronicamente, il che significa che il programma si bloccherà fino a quando la verifica non è stata completata. Questo potrebbe causare rallentamenti nell'esecuzione del codice, soprattutto se si sta controllando una directory molto grande o una directory remota.

Inoltre, tenere presente che il metodo `existsSync()` controlla solo l'esistenza della directory e non ne garantisce la validità o l'accessibilità. Ciò significa che anche se il metodo restituisce `true`, potrebbe ancora verificarsi un errore durante l'accesso alla directory.

## Vedi anche

- Documentazione del metodo `existsSync()` del modulo `fs`: https://nodejs.org/api/fs.html#fs_fs_existssync_path
- Tutorial su come controllare la presenza di una directory in JavaScript: https://www.digitalocean.com/community/tutorials/nodejs-check-if-file-or-directory-exists