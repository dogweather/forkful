---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie pliku tekstowego polega na zbieraniu i interpretowaniu danych zawartych w pliku. Programiści robią to, by manipulować informacjami, przeprowadzać analizy danych, stanowić źródło danych do pracy aplikacji i wiele innych.

## Jak to zrobić:

Podstawowy przykład odczytu pliku tekstowego w Javascript wykorzystuje 'fs' (moduł systemu plików) Node.js.

```Javascript
const fs = require('fs'); 

fs.readFile('test.txt', 'utf8', function(err, data){ 
    if(err) throw err; 
    console.log(data); 
}); 
```
Gdy uruchomisz ten kod, zobaczysz zawartość pliku "test.txt" wydrukowaną w konsoli.

## Głębsze spojrzenie:

Historia: W przeszłości, bez takich modułów jak 'fs' w Node.js, odczytywanie pliku tekstowego w Javascript na serwerze było trudniejsze i mniej wydajne.

Alternatywy: Inny popularny sposób to wykorzystanie funkcji asynchronicznej, co pozwala na bardziej liniowy, czytelny kod.

```Javascript
const fs = require('fs').promises;

async function readFileAsync() {
    let data = await fs.readFile('test.txt', 'utf8');
    console.log(data);
}

readFileAsync();
```

Szczegóły implementacji: Wybór między readFile i readFileSync (synchroniczna wersja) zależy od sytuacji. readFile jest nieblokująca, co oznacza, że nie czeka na zakończenie odczytu pliku przed przejściem do następnego zadania. readFile jest lepsza w środowiskach wielu użytkowników, gdzie blokowanie może prowadzić do problemów z wydajnością. 

## Zobacz również:

1. [Dokumentacja Node.js 'fs'](https://nodejs.org/api/fs.html)
2. [Poradnik MDN JavaScript](https://developer.mozilla.org/pl/docs/Learn/JavaScript)
3. [Czytanie i pisanie plików w Node.js - Stackabuse](https://www.stackabuse.com/read-files-with-node-js/)
4. [Asynchronous Javascript: From Callback Hell to Async and Await - Twilio](https://www.twilio.com/blog/2015/10/asyncawait-the-hero-javascript-deserved.html)