---
title:                "TypeScript: Łączenie ciągów znaków"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Concatenation jest niezbędnym elementem w programowaniu TypeScript i pozwala na łączenie ciągów znaków, czyli tekstowych danych, w jedną całość. Jest to szczególnie przydatne w przypadku, gdy potrzebujemy stworzyć dynamiczny komunikat lub wyświetlić dane użytkownika.

## Jak to zrobić

W TypeScript, możemy łatwo połączyć dwa lub więcej ciągów znaków przy użyciu operatora `+` lub metody `concat()`.

```TypeScript
const firstName: string = "Jan";
const lastName: string = "Kowalski";

// używając operatora +
const fullName: string = firstName + " " + lastName;
console.log(fullName); // output: "Jan Kowalski"

// używając metody concat()
const fullName2: string = firstName.concat(" ", lastName);
console.log(fullName2); // output: "Jan Kowalski"
```

W powyższym przykładzie widzimy, że możemy również połączyć więcej niż dwa ciągi znaków jednocześnie.

```TypeScript
const language1: string = "Type";
const language2: string = "Script";
const language3: string = "JavaScript";

// łączenie trzech ciągów znaków
const languages: string = language1 + language2 + language3;
console.log(languages); // output: "TypeScriptJavaScript"
```

Możemy również wykorzystać interpolację szablonową (template literals), aby łączyć ciągi znaków w bardziej wygodny sposób.

```TypeScript
const animal: string = "pies";
const sound: string = "szczeka";

// interpolacja szablonowa
const sentence: string = `Mój ${animal} ${sound}.`;
console.log(sentence); // output: "Mój pies szczeka."
```

## Deep Dive

Podczas łączenia ciągów znaków, ważne jest, aby pamiętać, że wynik będzie zawsze typu `string`, niezależnie od typu zmiennych, które łączymy.

```TypeScript
const number1: number = 2;
const number2: number = 3;

const result1: string = number1 + number2;
console.log(result1); // output: "5"

const result2: string = "2" + "3";
console.log(result2); // output: "23"
```

Ponadto, łącząc ciągi znaków, możemy również użyć metod takich jak `slice()` czy `substring()` aby manipulować poszczególnymi częściami łączonego tekstu.

```TypeScript
const sentence: string = "To jest przykładowe zdanie.";

// używając metody slice()
const part1: string = sentence.slice(0, 8);
console.log(part1); // output: "To jest"

// używając metody substring()
const part2: string = sentence.substring(10, 19);
console.log(part2); // output: "przykładowe"
```

Ważne jest również, aby pamiętać o znakach specjalnych, takich jak `\n` czy `\t`, które mogą być również łączone w ciągi znaków.

## Zobacz też

- [Dokumentacja TypeScript - String Operators](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string-operators)
- [W3Schools - TypeScript String Methods](https://www.w3schools.com/JSREF/jsref_obj_string.asp)

Dzięki połączeniu ciągów znaków, możemy skutecznie manipulować tekstowymi danymi w naszych programach. Pamiętajmy jednak o odpowiedniej manipulacji i użyciu odpowiednich metod, aby uniknąć błędów.