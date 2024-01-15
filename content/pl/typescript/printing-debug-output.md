---
title:                "Wydrukowanie wyników debugowania"
html_title:           "TypeScript: Wydrukowanie wyników debugowania"
simple_title:         "Wydrukowanie wyników debugowania"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Debugowanie jest nieodłączną częścią procesu programowania. W tym artykule dowiesz się, dlaczego warto wykorzystywać drukowanie outputu w procesie debugowania w języku TypeScript.

## Jak to zrobić

TypeScript jest obecnie jednym z najpopularniejszych języków programowania na świecie. Drukowanie outputu jest jedną z najprostszych i najbardziej skutecznych metod debugowania kodu. Poniżej przedstawiamy przykładowy kod w języku TypeScript, który wykorzystuje drukowanie outputu do zidentyfikowania błędów:

```TypeScript
let firstName: string = "John";
let lastName: string = "Doe";

console.log(`Hello ${firstName} ${lastName}!`);
```

Output: Hello John Doe!

W tym przykładzie użyliśmy funkcji `console.log()`, która wypisuje podane wyrażenie w konsoli. Można również wykorzystać funkcję `console.debug()` do drukowania informacji debugujących, takich jak wartości zmiennych i etapy działania programu.

## Deep Dive

Drukowanie outputu jest bardzo przydatne w przypadku, gdy potrzebujemy podglądu wartości zmiennych w trakcie działania programu. Może również pomóc w identyfikacji błędów, szczególnie w przypadku bardziej skomplikowanych programów.

Istnieją również biblioteki i narzędzia, które umożliwiają bardziej rozbudowane drukowanie outputu, takie jak `debug` lub `log4js`. Dzięki nim można dodawać tagi, kategorie i poziom logów, co ułatwia śledzenie i analizowanie outputu.

## Zobacz również

- [(Oficjalna dokumentacja TypeScript)](https://www.typescriptlang.org/docs/)
- [(In-depth tutorial on TypeScript debugging)](https://blog.bitsrc.io/debugging-typescript-in-visual-studio-code-2152ac438943)
- [(Tutorial on using log4js with TypeScript)](https://www.digitalocean.com/community/tutorials/how-to-use-log4js-logging-tools-with-typescript)