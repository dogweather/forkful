---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Javascript: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu jest potrzebne dostęp do argumentów wprowadzonych przez użytkownika z wiersza poleceń. Pisanie programów z wykorzystaniem argumentów wiersza poleceń może znacznie ułatwić życie programisty i sprawić, że nasz kod będzie bardziej elastyczny i konfigurowalny. W tym artykule dowiesz się, dlaczego warto poznać umiejętność czytania argumentów wiersza poleceń w języku Javascript.

## Jak to zrobić

W języku Javascript, argumenty wiersza poleceń są dostępne dzięki obiektowi `process.argv`. Jest to tablica, która przechowuje wszystkie argumenty wprowadzone przez użytkownika poza standardowym wierszem poleceń. Przykładowo:

```Javascript
console.log(process.argv[0]); // wyświetla ścieżkę do wiersza poleceń
console.log(process.argv[1]); // wyświetla ścieżkę do pliku, w którym został uruchomiony program
console.log(process.argv[2]); // wyświetla pierwszy argument wpisany przez użytkownika
```

Aby przetestować to w praktyce, stwórzmy plik `index.js` z następującym kodem:

```Javascript
console.log("Wprowadzone argumenty: ", process.argv.slice(2)); // wyświetla wszystkie argumenty wprowadzone przez użytkownika
```

Teraz z wiersza poleceń wykonajmy polecenie `node index.js argument1 argument2` i zobaczymy, że zostaną wyświetlone wszystkie argumenty wprowadzone przez użytkownika poza argumentami standardowego wiersza poleceń, czyli `argument1` oraz `argument2`, wraz z informacją "Wprowadzone argumenty:".

## Deep Dive

W przypadku, gdy program wymaga bardziej skomplikowanej opcji wprowadzania argumentów, można skorzystać z narzędzia `yargs`. Jest to biblioteka, która ułatwia parsowanie i zarządzanie argumentami wiersza poleceń. Poniżej znajduje się przykład użycia biblioteki `yargs`:

```Javascript
const yargs = require('yargs');

const options = yargs
  .usage("Użycie: -n <użytkownik>")
  .option("n", { 
    alias: "użytkownik", 
    describe: "Podaj użytkownika", 
    type: "string", 
    demandOption: true 
  })
  .argv;

const użytkownik = options.użytkownik;
console.log("Witaj, " + użytkownik + "!");

```

Po uruchomieniu tego programu za pomocą polecenia `node index.js -n Jan`, zostaniemy przywitani przez imię wprowadzone jako argument `użytkownik`.

## See Also

Jeśli jesteś zainteresowany dalszą nauką o argumentach wiersza poleceń w języku Javascript, polecamy zapoznanie się z następującymi linkami:

- [Dokumentacja o obiekcie process w Node.js](https://nodejs.org/api/process.html)
- [Dokumentacja biblioteki yargs](https://www.npmjs.com/package/yargs)