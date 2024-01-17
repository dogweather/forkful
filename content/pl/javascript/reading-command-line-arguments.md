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

Często podczas pisania kodu w JavaScriptzie potrzebujemy, aby nasz program przyjmował argumenty z linii poleceń. W artykule tym dowiesz się, czym jest odczytywanie argumentów z linii poleceń i dlaczego jest to przydatne dla programistów.

## Co & Dlaczego?
Odczytywanie argumentów z linii poleceń jest procesem pozwalającym na przekazanie danych do naszego programu podczas jego uruchamiania. Dzięki temu programista może ustawić różne wartości dla tych argumentów, co pozwala na bardziej elastyczne i dostosowane działanie programu.

## Jak to zrobić:
Aby odczytać argumenty z linii poleceń w JavaScript, możemy użyć obiektu `process.argv`. Przykładowy kod wygląda następująco:

```Javascript
const args = process.argv;
console.log(args);
```

Jeśli uruchomimy ten kod z argumentami `node index.js one two three`, to otrzymamy następujący wynik:

```Javascript
["path/to/node", "path/to/index.js", "one", "two", "three"]
```

Pierwsze dwa elementy to ścieżki do plików `node` i `index.js`, a kolejne elementy to przekazane argumenty.

## Głębsze spojrzenie:
Odczytywanie argumentów z linii poleceń jest popularną praktyką w wielu językach programowania. Jeden z najwcześniejszych języków, który pozwalał na to, to C. Wielu programistów używa tego mechanizmu do przekazywania opcji do swojego programu, takich jak np. tryb debugowania, ścieżki do plików, czy ustawienia sieciowe.

Alternatywnym sposobem odczytywania argumentów jest korzystanie z biblioteki `yargs`, która zapewnia bardziej zaawansowane funkcjonalności, takie jak obsługa flag i argumentów pozycyjnych.

W implementacji odczytywania argumentów z linii poleceń, warto pamiętać o sprawdzeniu poprawności wprowadzonych danych, aby uniknąć błędów w programie.

## Zobacz też:
- [Dokumentacja biblioteki yargs] (https://www.npmjs.com/package/yargs)
- [Artykuł na temat odczytywania argumentów z linii poleceń w języku Python] (https://realpython.com/python-command-line-arguments/)