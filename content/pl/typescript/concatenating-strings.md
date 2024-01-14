---
title:    "TypeScript: Laczenie ciagow znakow"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Jedną z podstawowych funkcji TypeScript jest łączenie ciągów znaków (strings). Jest to niezwykle przydatne narzędzie, ponieważ pozwala na łączenie różnych części tekstu w celu stworzenia pełnego wyrażenia lub zdania. W tym blogu omówimy dlaczego warto używać tej funkcji i jak to zrobić w praktyce.

## Jak to zrobić

Aby połączyć dwa ciągi znaków w TypeScript, musimy wykorzystać operator "+" lub metodę "concat()". Przykładowy kod wygląda następująco:

```TypeScript
let str1: string = "Hello";
let str2: string = "world";

// Użycie operatora "+"
let result1: string = str1 + " " + str2;
console.log(result1); // Output: Hello world

// Użycie metody "concat()"
let result2: string = str1.concat(" ", str2);
console.log(result2); // Output: Hello world
```

W obu przypadkach operator "+" oraz metoda "concat()" łączą dwa ciągi znaków w jeden, dodając pomiędzy nimi spacje. Możemy również użyć tych funkcji do łączenia większej ilości ciągów znaków.

## Deep Dive

Warto zauważyć, że w TypeScript możemy również łączyć inne typy danych, takie jak liczby czy boole. Jednak w takich przypadkach aplikacja konwerteruje te wartości na ciągi znaków przed wykonaniem operacji łączenia. W przypadku operatora "+" należy pamiętać o umieszczeniu wszystkich wartości w nawiasach, aby uniknąć nieoczekiwanych wyników.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o łączeniu ciągów znaków w TypeScript, polecamy zapoznanie się z poniższymi linkami:

- [Oficjalna dokumentacja TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Tutorial z wykorzystaniem operatora "+" i metody "concat()"](https://www.tutorialspoint.com/typescript/typescript_strings.htm)
- [Inny sposób łączenia tekstów w TypeScript](https://www.pluralsight.com/guides/concatenating-strings-in-typescript)

Dziękujemy za uwagę i życzmy powodzenia w nauce łączenia ciągów znaków w TypeScript!