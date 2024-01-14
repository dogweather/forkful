---
title:    "TypeScript: Znajdując długość ciągu znaków"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego 

Często podczas programowania, jeden z problemów z jakimi możemy się spotkać to konieczność znalezienia długości ciągu znaków. Może to być potrzebne do walidacji danych czy też do manipulacji tekstem. W tym artykule dowiecie się jak w prosty sposób można znaleźć długość ciągu znaków w języku TypeScript.

## Jak to zrobić

Aby znaleźć długość ciągu znaków w TypeScript, możemy wykorzystać wbudowaną funkcję `length`.

```TypeScript
let string = "Programowanie w TypeScript jest super!";
console.log(string.length); // Output: 36
```

W powyższym przykładzie stworzyliśmy zmienną `string` zawierającą nasz ciąg znaków, a następnie wywołaliśmy na niej funkcję `length`, która zwróciła nam długość ciągu.

Możemy również wykorzystać tę funkcję w połączeniu z metodą `toString()` aby znaleźć długość dowolnego obiektu typu `string`.

```TypeScript
let username = "John";
console.log(username.toString().length); // Output: 4
```

## Deep Dive

Funkcja `length` zwraca liczbę znaków w ciągu, a nie indeks ostatniego znaku. Liczenie zaczyna się od zera, więc ostatni znak będzie miał indeks o jeden mniejszy niż wartość zwrócona przez funkcję `length`.

```TypeScript
let string = "To jest przykład";
console.log(string.length); // Output: 16
console.log(string[15]); // Output: d (to jest ostatni znak)
console.log(string[16]); // Output: undefined
console.log(string[string.length - 1]); // Output: d (również ostatni znak)
```

Funkcja `length` nie jest dostępna dla typów danych opartych na obiektach, takich jak `Object` czy `Array`. Jeśli chcemy znaleźć długość tablicy, musimy wykorzystać pole `length`.

```TypeScript
let array = [1, 2, 3, 4, 5];
console.log(array.length); // Output: 5
```

## Zobacz również

- [Oficjalna dokumentacja TypeScript](https://www.typescriptlang.org/docs/)
- [Funkcja length() w języku JavaScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String/length)