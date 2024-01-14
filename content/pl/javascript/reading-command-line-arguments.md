---
title:    "Javascript: Odczytywanie argumentów wiersza poleceń"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą JavaScript lub stawiasz pierwsze kroki w nauce tego języka, zapewne słyszałeś o możliwości czytania argumentów wiersza poleceń. W tym artykule dowiesz się dlaczego jest to ważna umiejętność, a także jak ją wykorzystać w swoim kodzie.

## Jak to zrobić

Czytanie argumentów wiersza poleceń może być bardzo przydatne, gdy chcesz umożliwić użytkownikom wprowadzanie zmiennych lub opcji do swojego programu. W JavaScript możesz to zrobić za pomocą obiektu `process.argv`, który zawiera tablicę z argumentami podanymi w wierszu poleceń. Poniżej przedstawiam prosty przykład, który wypisuje wszystkie argumenty w konsoli:

```Javascript
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});
```

Jeśli uruchomisz ten kod z dwoma argumentami (np. `node index.js hello world`), w konsoli zostanie wyświetlona następująca informacja:
```
0: /usr/local/bin/node
1: /path/to/index.js
2: hello
3: world
```

Jak widać, pierwsze dwa argumenty dotyczą samego uruchomienia programu, a kolejne wyświetlają zmienne przekazane przez użytkownika.

## Głębszy krok

Jeśli chcesz jeszcze bardziej dokładnie zarządzać przekazywanymi argumentami, możesz wykorzystać moduł `commander`. Pozwala on na definiowanie własnych opcji i argumentów, a także wyświetlanie informacji o poprawnym używaniu programu. W poniższym przykładzie sprawdzamy, czy użytkownik wprowadził flagę `-m` z argumentem, a następnie wyświetlamy ten argument w konsoli:

```Javascript
const program = require('commander');

program
  .option('-m, --message <value>', 'Specify a custom message')
  .parse(process.argv);

if (program.message) {
  console.log('Your message is:', program.message);
} else {
  console.log('Please specify a message with the -m flag');
}
```

Teraz, jeśli uruchomisz ten kod z argumentem `-m hello`, na ekranie pojawi się informacja `Your message is: hello`.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o czytaniu argumentów wiersza poleceń w JavaScript, możesz zapoznać się z poniższymi artykułami:

- [Dokumentacja procesu w Node.js](https://nodejs.org/api/process.html#process_process_argv)
- [Poradnik programisty - argumenty wiersza poleceń w JavaScript](https://www.digitalocean.com/community/tutorials/nodejs-command-line-options-with-commander-js)