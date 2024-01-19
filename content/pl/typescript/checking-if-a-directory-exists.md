---
title:                "Sprawdzanie, czy katalog istnieje"
html_title:           "Lua: Sprawdzanie, czy katalog istnieje"
simple_title:         "Sprawdzanie, czy katalog istnieje"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Sprawdzanie, czy katalog istnieje w TypeScript

## Co i Dlaczego?

Sprawdzanie, czy katalog istnieje, oznacza potwierdzenie, czy określona ścieżka katalogu istnieje w systemie plików. Programiści robią to, aby uniknąć błędów podczas manipulowania plikami i katalogami.

## Jak to zrobić:

Typowo w TypeScript, używa się API Node.js do interakcji z systemem plików. Oto prosty przykład:

```TypeScript
import * as fs from 'fs';

let dirPath = "/ścieżka/do/katalogu";

if(fs.existsSync(dirPath)) {
    console.log("Katalog istnieje.");
} else {
    console.log("Katalog nie istnieje.");
}
```

Podczas uruchamiania powyższego kodu, zostanie wyświetlony komunikat "Katalog istnieje" jeśli dany katalog istnieje, a jeśli nie - "Katalog nie istnieje".

## Szerzej:

Historia: W początkach Node.js (tzn. w JavaScript), podejście było takie samo, ale zamiast 'import’ użyliśmy 'require'.

Alternatywy: Istnieje wiele bibliotek i modułów zewnętrznych do manipulacji systemem plików, takich jak 'fs-extra', które dostarcza liczne funkcje.

Szczegóły implementacji: Metoda 'fs.existsSync' działa synchronicznie i blokuje wykonywanie pozostałego kodu do momentu zakończenia sprawdzania. Jeśli potrzebujesz wykonania asynchronicznego, skorzystaj z 'fs.stat' lub 'fs.access'.

## Zobacz także: 

Projekt Node.js: https://nodejs.org/dist/latest-v14.x/docs/api/fs.html#fs_fs_existssync_path

"Node.js file system documentation":https://nodejs.org/api/fs.html

"Understanding the basics of file system in Node.js": https://www.tutorialsteacher.com/nodejs/nodejs-file-system