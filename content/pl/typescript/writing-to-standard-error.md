---
title:                "Pisanie do standardowego błędu"
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
"Standard error", czyli "stderr", to oddzielny kanał wyjściowy przeznaczony dla komunikatów o błędach i logów diagnostycznych. Programiści używają go, by oddzielić normalne wyjście programu od informacji o ewentualnych problemach, co ułatwia debugowanie i przetwarzanie wyników.

## Jak to zrobić?
```TypeScript
// Wysłanie komunikatu o błędzie do standard error w TypeScript
console.error('Wystąpił błąd!');

// Alternatywnie, używanie process.stderr.write
process.stderr.write('To jest szczegółowy opis błędu.\n');
```
Wyjście:
```
Wystąpił błąd!
To jest szczegółowy opis błędu.
```

## Głębiej w temat
Historia "stderr" sięga czasów Unix'a, gdzie podzielono strumienie wyjściowe, by lepiej zarządzać komunikatami i danymi. Alternatywą dla "stderr" jest "stdout" (standard output), ale jak wspomniano, jego użycie jest zarezerwowane dla normalnego wyjścia programu, nie dla błędów. W implementacji TypeScript, podobnie jak w JavaScript, pisząc pod Node.js, używamy globalnych obiektów `console` i `process` do interakcji ze strumieniami. Strumień "stderr" jest synchroniczny, co oznacza, że program czeka na jego opróżnienie przed przystąpieniem do kolejnych instrukcji.

## Zobacz też
- [Node.js process.stderr documentation](https://nodejs.org/api/process.html#process_process_stderr)
- [Console.error() on MDN](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [The Unix philosophy](https://en.wikipedia.org/wiki/Unix_philosophy)
