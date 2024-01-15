---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Javascript: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego ktoś powinien czytać plik tekstowy w języku JavaScript? W dzisiejszych czasach, niezależnie od tego czy jesteś programistą, analitykiem danych czy blogerem, prawdopodobnie masz do czynienia z plikami tekstowymi. Umiejętność czytania tych plików jest niezbędna do wykonywania wielu zadań, dlatego warto poznać możliwości JavaScript w tym zakresie.

## Jak To Zrobić

Pierwszym krokiem jest otworzenie pliku tekstowego. Do tego celu wykorzystaj funkcję `fs.readFileSync()` z wbudowanego modułu `fs`. Przyjmuję ona dwa argumenty: ścieżkę do pliku oraz opcję kodowania. Przykładowy kod wygląda następująco:

```Javascript
const fs = require('fs');
const text = fs.readFileSync('plik.txt', 'utf8');
```

Następnie możesz wykorzystać metodę `split()` do podzielenia tekstu na linie lub słowa. Na przykład, aby wyświetlić zawartość pliku linia po linii, możesz użyć poniższego kodu:

```Javascript
const lines = text.split('\n');

for (let line of lines) {
  console.log(line);
}
```

Jeśli chcesz wyświetlić zawartość pliku słowo po słowie, możesz wykorzystać metode `split()` jeszcze raz, używając jako separatorów spacji lub innych znaków. Przykładowy kod wygląda tak:

```Javascript
const words = text.split(' ');

for (let word of words) {
  console.log(word);
}
```

Jeśli chcesz uzyskać informacje o ilości linii lub słów w pliku, możesz wykorzystać odpowiednio `lines.length` lub `words.length`. Aby zapisać zawartość pliku do nowego pliku, możesz użyć metody `writeFileSync()` z modułu `fs`:

```Javascript
fs.writeFileSync('nowy_plik.txt', text);
```

## Deep Dive

W przypadku, gdy masz do czynienia z bardzo dużymi plikami tekstowymi, czytanie i przetwarzanie ich może być czasochłonne i prowadzić do błędów pamięci. W takiej sytuacji warto rozważyć wykorzystanie modułu `readline`, który pozwala na wczytywanie pliku linia po linii, bez konieczności wczytywania całego pliku do pamięci. Przykładowy kod wykorzystujący ten moduł wygląda następująco:

```Javascript
const fs = require('fs');
const readline = require('readline');

const readInterface = readline.createInterface({
  input: fs.createReadStream('plik.txt'),
});

readInterface.on('line', function(line) {
  console.log(line);
});
```

Jeśli chcesz przetworzyć i wybrać tylko niektóre linie z pliku, możesz wykorzystać funkcję wyrażenia regularnego z metodą `test()`. Na przykład, aby wybrać tylko linie zawierające wyraz "JavaScript", możesz użyć kodu:

```Javascript
const fs = require('fs');
const readline = require('readline');

const readInterface = readline.createInterface({
  input: fs.createReadStream('plik.txt'),
});

readInterface.on('line', function(line) {
  let pattern = /JavaScript/;
  if (pattern.test(line)) {
    console.log(line);
  }
});
```

## Zobacz Również

- [Dokumentacja modułu fs](https://nodejs.org/api/fs.html)
- [Dokumentacja modułu readline](https://nodejs.org/api/readline.html)
- [Tutorial na temat obsługi plików tekstowych w JavaScript](https://www.digitalocean.com/community/tutorials/how-to-handle-files-in-node-js)