---
title:                "Interpolacja ciągu znaków"
html_title:           "TypeScript: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

Czesc programisci! Dzisiaj omówimy czym jest i dlaczego warto uzywac interpolacji lancucha znakow w jezyku TypeScript.

## Co i Dlaczego?

Interpolacja lancucha znakow jest procesem, w którym mozemy wstawic wartosci zmiennych do naszego lancucha znakow, aby dynamicznie tworzyc wiadomosci lub komunikaty. Jest to bardzo przydatne, poniewaz pozwala nam tworzyc bardziej czytelny i elastyczny kod.

## Jak to zrobic:

Mozemy uzywac interpolacji lancucha znakow w TypeScript, uzywajac znaku dolara i nawiasu klamrowego, aby wstawic wartosc zmiennej. Oto przyklad:

```TypeScript
let name: string = "John";
console.log(`Welcome, ${name}!`);
```

Output: Welcome, John!

Mozemy rowniez uzywac interpolacji lancucha znakow w bardziej skomplikowanych przypadkach. Na przyklad, mozemy dodawac operacje logiczne:

```TypeScript
let num1: number = 5;
let num2: number = 10;
console.log(`Sum of ${num1} and ${num2} is ${num1 + num2}.`);
```

Output: Sum of 5 and 10 is 15.

## Glebokie Zaglebie:

Interpolacja lancucha znakow nie jest nowym konceptem i pierwotnie zostala wprowadzona w jezyku Perl. Jednak obecnie jest ona obslugiwana przez wiele jezykow programowania, w tym przez TypeScript. Alternatywnym podejsciem do interpolacji lancucha znakow jest uzycie operatora plus (+) lub metody concat, ale jest to mniej czytelne i wymaga wiekszej ilosci kodu.

Podczas implementacji interpolacji lancucha znakow w TypeScript, wartosci zmiennych sa automatycznie konwertowane na lancuchy znakow. Ponadto, znaki specjalne, takie jak nowa linia lub tabulacja, sa rowniez uwzgledniane w wynikowym lancuchu znakow.

## Zobacz Takze:

Mozesz przeczytac wiecej o interpolacji lancucha znakow w dokumentacji TypeScript: 

https://www.typescriptlang.org/docs/handbook/basic-types.html#template-strings