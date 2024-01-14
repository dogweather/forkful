---
title:                "Javascript: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Dlaczego odczytywanie plików tekstowych jest ważne dla programistów Javascript?

Odczytywanie plików tekstowych jest ważną częścią pracy programisty Javascript. Pliki tekstowe są powszechnie wykorzystywane jako źródło danych do przetwarzania przez programy komputerowe. Dlatego ważne jest, aby umieć odczytywać i przetwarzać te pliki w swoim kodzie Javascript.

## Jak to zrobić?

Aby odczytać plik tekstowy w Javascript, musimy użyć specjalnych funkcji dostępnych w module "fs". Najpierw musimy zaimportować ten moduł za pomocą polecenia "require".

```
const fs = require('fs');
```

Następnie możemy użyć funkcji "readFile" do odczytania pliku tekstowego. Funkcja ta przyjmuje dwa argumenty - ścieżkę do pliku oraz funkcję zwrotną, która zostanie wywołana po zakończeniu odczytywania pliku.

```
fs.readFile('tekstowy_plik.txt', (err, data) => {
  if(err) {
    console.log('Wystąpił błąd podczas czytania pliku: ' + err);
  } else {
    console.log(data);
  }
});
```

Następnie możemy przetworzyć odczytane dane, używając metod dostępnych na obiekcie "data". Na przykład, jeśli chcemy wyświetlić odczytane dane jako tekst, możemy użyć metody "toString".

```
console.log(data.toString());
```

## Pełne zanurzenie

Odczytywanie plików tekstowych może być czasem skomplikowane, szczególnie gdy plik jest bardzo duży lub zawiera specjalne znaki. Jednak w Javascript istnieje wiele funkcji i metod, które ułatwiają ten proces.

Funkcja "readFile" przyjmuje również trzeci argument - opcje. Możemy użyć tej opcji, aby określić format odczytywanego pliku, na przykład "utf8" lub "ascii".

```
fs.readFile('tekstowy_plik.txt', 'utf8', (err, data) => {
  // kod do przetwarzania danych
});
```

Jeśli chcemy odczytywać plik tekstowy w kawałkach, a nie jako całość, możemy użyć funkcji "createReadStream". Ta metoda pozwala nam określić rozmiar i przesuwane przez nas kawałki.

```
const readStream = fs.createReadStream('tekstowy_plik.txt', {
  highWaterMark: 64,
  start: 100,
  end: 200
});
```

Mamy również dostęp do wielu innych metod, takich jak "readFileSync", "read", czy "readlink", które pozwalają nam na odczytywanie plików w różnych scenariuszach.

# Zobacz również

- [Dokumentacja Node.js: Moduł "fs"](https://nodejs.org/api/fs.html)
- [MDN: Praca z plikami w Javascript](https://developer.mozilla.org/pl/docs/Learn/JavaScript/Client-side_web_APIs/Fetching_data#Working_with_files_in_JavaScript)