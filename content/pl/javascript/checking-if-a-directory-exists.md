---
title:    "Javascript: Sprawdzenie czy istnieje katalog"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego?

Jeśli jesteś programistą lub hobbystycznym programistą, z pewnością spotkałeś się z sytuacją, w której musiałeś sprawdzić, czy dany folder lub ścieżka istnieje w systemie plików. W tym artykule dowiesz się, dlaczego jest to ważna umiejętność i jak ją wykorzystać w swoim kodzie.

## Jak to zrobić?

Sprawdzenie, czy dany katalog istnieje w systemie plików, może być potrzebne w wielu sytuacjach, na przykład przy tworzeniu aplikacji do zarządzania plikami lub przy automatycznej obsłudze plików. W JavaScript jest kilka sposobów, aby to osiągnąć.

Pierwszym sposobem jest użycie wbudowanej funkcji `fs.existsSync()`, która zwraca wartość logiczną `true`, jeśli dany katalog istnieje, lub `false`, jeśli nie istnieje. Przykładowy kod wyglądałby następująco:

```Javascript
var fs = require('fs');
var directoryPath = 'ścieżka/do/katalogu';

if (fs.existsSync(directoryPath)) {
    console.log('Katalog istnieje.');
} else {
    console.log('Katalog nie istnieje.');
}
```

Drugim sposobem jest użycie funkcji `fs.stat()` i obsłużenie błędu w przypadku, gdy dany katalog nie istnieje. Przykładowy kod wyglądałby następująco:

```Javascript
var fs = require('fs');
var directoryPath = 'ścieżka/do/katalogu';

fs.stat(directoryPath, function(err, stats) {
    if (err) {
        console.log('Katalog nie istnieje.');
    } else {
        console.log('Katalog istnieje.');
    }
});
```

## Głębsze zagadnienia

Podczas tworzenia aplikacji z wykorzystaniem sprawdzania istnienia katalogów, możesz natknąć się na kilka wyzwań. Na przykład, funkcja `fs.existsSync()` może zwrócić `true` nawet w przypadku, gdy podana ścieżka wskazuje na plik, a nie katalog. W takiej sytuacji, można wykorzystać funkcję `fs.statSync()`, aby dokładnie określić, czy dany element jest katalogiem czy plikiem.

Możesz również wykorzystać `fs.mkdirSync()` lub `fs.mkdir()` w połączeniu z warunkiem sprawdzającym istnienie katalogu, aby utworzyć go tylko w przypadku, gdy jeszcze nie istnieje.

## Zobacz również

Jeśli chcesz dowiedzieć się więcej o operacjach na plikach i katalogach w JavaScript, polecamy Ci zapoznać się z poniższymi artykułami:

- [Manipulowanie plikami przy użyciu Node.js](https://www.blogName.com/manipulowanie-plikami-nodejs)
- [Tworzenie plików za pomocą Node.js](https://www.blogName.com/tworzenie-plikow-nodejs)
- [Node.js - Sprawdzanie czy plik istnieje](https://www.blogName.com/nodejs-sprawdzanie-czy-plik-istnieje)