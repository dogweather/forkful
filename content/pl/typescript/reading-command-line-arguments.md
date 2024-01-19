---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obsługa argumentów z linii poleceń to proces odczytywania parametrów przekazywanych do twojego programu podczas jego uruchamiania. Programiści robią to, by umożliwić personalizację zachowania programów bez konieczności edytowania kodu.

## Jak to zrobić:

Można odczytać argumenty lini poleceć w TypeScript za pomocą wbudowanej globalnej tablicy `process.argv`. Poniżej znajduje się przykład:

```TypeScript
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});
```

Po uruchomieniu powyższego skryptu z argumentami, output wyglądałby następująco:

```TypeScript
// $ node .\index.ts arg1 arg2
0: node
1: .\index.ts
2: arg1
3: arg2
```

Powyższy output pokazuje, że pierwsze dwa elementy w `process.argv` zawsze są ścieżką do interpretera node.js (`node`) oraz ścieżką do uruchomionego skryptu.

## Pogłębione spojrzenie:

Odczytywanie argumentów linii poleceń jest techniką używaną od początków programowania - pochodzi ona z języka C.

Egzystują alternatywne metody obsługi argumentów linii poleceć, takie jak używanie paczki zewnętrznej jak `yargs` czy `commander.js`, które dostarczają bardziej rozbudowanej funkcjonalności.

Jak już wspomnieliśmy, w TypeScript (podobnie jak w Node.js), `process.argv` jest domyślnym sposobem odczytywania tych argumentów. Jest to tablica, która zawiera elementy przekazane do skryptu podczas wywołania. Pierwsze dwa elementy to ścieżka do interpretera Node.js i ścieżka do uruchamionego skryptu, a reszta to przekazane argumenty.

## Zobacz też:

Jeżeli jesteś zainteresowany w pełnym zrozumieniem argumentów linii poleceć w TypeScript, zapoznaj się z następującymi źródłami:

- [Dokumetacja Node.js 'process.argv'](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Paczki dostarczające dodatkowe opcje: 'yargs'](https://www.npmjs.com/package/yargs)
- ['commander.js'](https://www.npmjs.com/package/commander)