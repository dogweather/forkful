---
title:                "Prowadzenie sprawdzania istnienia katalogu"
html_title:           "Javascript: Prowadzenie sprawdzania istnienia katalogu"
simple_title:         "Prowadzenie sprawdzania istnienia katalogu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie, czy katalog istnieje, jest ważnym elementem programowania w języku Javascript. Pozwala to programistom na upewnienie się, że określona ścieżka jest poprawna i dostępna, co może pomóc uniknąć błędów i problemy z wykonywaniem innych operacji na plikach.

## Jak:
Aby sprawdzić, czy katalog istnieje w Javascript, można skorzystać z funkcji ```fs.existsSync()```, która sprawdza, czy dany katalog lub plik istnieje. Należy jednak pamiętać, że ta funkcja jest dostępna tylko w środowisku Node.js. Poniższy przykład kodu pokazuje, jak użyć tej funkcji w praktyce:

```Javascript
const fs = require('fs');
const path = './myDirectory';

if (fs.existsSync(path)){
  console.log('Katalog istnieje');
} else {
  console.log('Katalog nie istnieje');
}
```

W przypadku, gdy funkcja ```fs.existsSync()``` zwraca wartość ```true```, oznacza to, że podana ścieżka istnieje. W przeciwnym razie, funkcja zwraca ```false```. W ten sposób, można wybrać odpowiednie działania w przypadku, gdy sprawdzany katalog istnieje lub nie.

## Deep Dive
Funkcja ```fs.existsSync()``` została wprowadzona w wersji 0.8 języka Node.js w celu sprawdzenia, czy plik lub katalog istnieje. Alternatywnie, można również skorzystać z funkcji ```fs.stat()```, która pozwala na pobranie informacji o danym pliku lub katalogu, w tym czy istnieje.

Oprócz tego, pojęcie sprawdzania istnienia katalogu jest związane z obsługą błędów. W przypadku, gdy katalog nie istnieje, może pojawić się wyjątek, dlatego ważne jest odpowiednie obsłużenie takiej sytuacji w kodzie.

## Zobacz także:
- Dokumentacja funkcji [fs.existsSync()](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- Porównanie funkcji ```fs.existsSync()``` i ```fs.stat()``` [w tym artykule na Medium](https://medium.com/@bryanjenningz/how-to-check-if-a-file-or-directory-exists-in-node-js-1235aefc7fa0)
- Inne metody obsługi plików i katalogów w Javascript, takie jak [listowanie plików](https://www.npmjs.com/package/lljkjj_a) czy [tworzenie katalogów](https://nodejs.org/api/fs.html#fs_fs_mkdir_path_options_callback)