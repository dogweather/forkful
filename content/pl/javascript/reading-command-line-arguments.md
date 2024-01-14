---
title:                "Javascript: Odczytywanie argumentów wiersza poleceń"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą JavaScript i chcesz poznać sposoby na odczytywanie argumentów wiersza poleceń, ten artykuł jest dla Ciebie! Poznasz tutaj różne metody, jakie można zastosować, aby wykorzystać argumenty wiersza poleceń w swoim kodzie.

## Jak to zrobić

Istnieje kilka sposobów odczytywania argumentów wiersza poleceń w JavaScript. Pierwszym z nich jest użycie wbudowanego obiektu process, który zawiera wszystkie informacje o aktualnym procesie, wraz z argumentami wiersza poleceń. Przykładowy kod wykorzystujący ten sposób wyglądałby tak:

```Javascript
// Użycie process.argv do odczytania argumentów wiersza poleceń
const args = process.argv.slice(2); // Pierwsze dwa elementy zwracanego tablicy to ścieżka do pliku uruchamiającego oraz ścieżka do interpretera
console.log(args); // Wyświetli tablicę z wszystkimi argumentami wiersza poleceń przekazanymi podczas uruchamiania programu
```

Kolejną metodą jest użycie biblioteki yargs, która upraszcza odczytywanie argumentów i pozwala na definiowanie argumentów w bardziej intuicyjny sposób. Przykładowy kod z wykorzystaniem yargs mógłby wyglądać następująco:

```Javascript
// Użycie biblioteki yargs do odczytania argumentów wiersza poleceń
const argv = require('yargs').argv; // Import biblioteki

console.log(argv); // W tym przypadku pierwsze dwa elementy nie są wyświetlane, tylko właściwe argumenty

// Definicja argumentów przy użyciu yargs
// Użycie --option=value lub --option wartość
argv.option1 // pierwszy sposób przekazania argumentu
argv.option2 // drugi sposób przekazania argumentu
```

## Głębszy zanurzenie

Za pomocą obu metod możesz odczytywać argumenty wiersza poleceń w swoim kodzie JavaScript. Jednak warto zauważyć pewne różnice między tymi sposobami. Użycie process.argv wymaga samodzielnej analizy tablicy zwracanej przez ten obiekt, co może być bardziej czasochłonne. Natomiast yargs pozwala na prostszą i bardziej czytelną definicję argumentów, jednak wymaga dodatkowej biblioteki. Możesz wybrać odpowiedni sposób w zależności od swoich preferencji i potrzeb.

## Zobacz także

- [Dokumentacja process.argv](https://nodejs.org/api/process.html#process_process_argv)
- [Dokumentacja yargs](https://yargs.js.org/)