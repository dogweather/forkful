---
title:                "TypeScript: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Concatenating stringów jest niezbędnym narzędziem w każdym języku programowania, a TypeScript nie jest tu wyjątkiem. W tym artykule dowiesz się, dlaczego warto nauczyć się tej operacji i jak ją wykonać.

## Jak To Zrobić

Aby połączyć dwa lub więcej stringów w TypeScript, używamy operatora `+` lub funkcji `concat()`. Przykładowe użycie wygląda następująco:

```TypeScript
let string1 = "Hello";
let string2 = "World";
console.log(string1 + " " + string2); // Output: Hello World

let string3 = string1.concat(string2);
console.log(string3); // Output: HelloWorld
```

W pierwszym przykładzie używamy operatora `+` do połączenia dwóch zmiennych typu string i wyświetlamy wynik za pomocą funkcji `console.log()`. W drugim przykładzie używamy funkcji `concat()` na zmiennej `string1` i przekazujemy do niej jako argument zmienną `string2`, a następnie wyświetlamy wynik. W obu przypadkach otrzymujemy pożądany rezultat - połączone stringi.

Warto również zauważyć, że używając operatora `+`, możemy łączyć nie tylko zmienne typu string, ale również inne typy danych, takie jak liczby czy booleany. Jednak w przypadku funkcji `concat()`, wszystkie argumenty muszą być typu string.

## Deep Dive

Podczas wykonywania operacji concatenacji, możemy również użyć interpolacji stringów, czyli wstawiania wartości zmiennych bezpośrednio wewnątrz stringa. W TypeScript służy do tego operator `${}`. Przykładowo:

```TypeScript
let name = "John";
let age = 25;

console.log(`${name} is ${age} years old.`); // Output: John is 25 years old.
```

Możemy również użyć funkcji `concat()` w połączeniu z interpolacją stringów, aby uzyskać bardziej elastyczne i czytelne rozwiązanie. Przykładowy kod wyglądałby tak:

```TypeScript
let name = "John";
let age = 25;

let sentence = `My name is ${name} and I am ${age} years old.`
let fullSentence = "Hello, ".concat(sentence);

console.log(fullSentence); // Output: Hello, My name is John and I am 25 years old.
```

Podczas łączenia stringów warto również uważać na formatowanie. Jeśli chcemy, aby wynik był w jednej linii, musimy odpowiednio zapisywać stringi lub użyć specjalnych funkcji, takich jak `trim()` czy `replace()`, aby usunąć dodatkowe białe znaki lub znaki specjalne.

## Zobacz Również

- [Dokumentacja TypeScript na temat operacji stringowych](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Tutorial na temat interpoalacji stringów w TypeScript](https://www.typescripttutorial.net/typescript-tutorial/typescript-string-interpolation/#:~:text=Interpolation%20is%20a%20way%20to,url%20or%20any%20other%20expression.)
- [Inne operacje na stringach w TypeScript](https://www.geeksforgeeks.org/typescript-string-operations/)