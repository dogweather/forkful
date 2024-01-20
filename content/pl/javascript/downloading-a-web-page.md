---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
 
Pobieranie strony internetowej to proces zapisywania wszystkich danych strony na dysku lokalnym - to tak, jakbyś robił zrzut ekranu, ale zapisywał całe jej "ciało". Programiści to robią, aby móc analizować strukturę strony, pracować nad jej zawartością offline, a także do celów testowania i debugowania.

## Jak to zrobić:

Zajmijmy się najprostszym sposobem na pobranie strony internetowej za pomocą Node.js i popularnej biblioteki `axios`. Pamiętaj, aby najpierw zainstalować `axios` za pomocą npm, wpisując w wierszu poleceń `npm install axios`.

```Javascript
var axios = require('axios');
var fs = require('fs');

axios.get('http://example.com').then((response) => {
    fs.writeFile('example.html', response.data, (err) => {
        if(err) console.log(err);
        console.log('Strona zapisana pomyślnie.');
    });
});
```

Taki kod pobierze stronę `http://example.com` i zapisze jej zawartość do pliku `example.html`.

## Pogłębione spojrzenie:

Historia pobierania stron internetowych sięga początków internetu - kiedy to programiści musieli nauczyć się, jak robic "migawki" stron internetowych i analizować je offline. Wykorzystywane do tego techniki ewoluowały na przestrzeni lat - od korzystania z surowego HTML do skomplikowanych bibliotek JavaScript.

Alternatywą dla `axios` może być `node-fetch`, biblioteka która korzysta z API Fetch, znanego przede wszystkim z przeglądarek internetowych. Wybór pomiędzy tymi dwoma zwykle zależy od osobistych preferencji.

Szczegóły implementacji "pobierania strony" mogą rożnić się w zależności od strony. Na przykład, niektóre strony mogą wymagać użycia cookies lub innych nagłówków HTTP, aby pobrać całą jej zawartość.

## Zobacz też:

1. Więcej o `axios`: [Axios na GitHub](https://github.com/axios/axios)
2. Więcej o `node-fetch`: [node-fetch na GitHub](https://github.com/node-fetch/node-fetch)
3. Metody `fs.writeFile()` i `fs.writeFileSync()` : [Dokumentacja Node.js](https://nodejs.org/api/fs.html#fs_file_system)