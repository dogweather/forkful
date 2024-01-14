---
title:    "TypeScript: Wyświetlanie danych wyjściowych z debugowania"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego warto wyświetlać informacje debugowania w TypeScript

Wyświetlanie informacji debugowania jest niezbędnym narzędziem dla każdego programisty. Dzięki temu możemy łatwiej zrozumieć działanie naszego kodu, śledzić ewentualne błędy i poprawiać je w szybkim tempie. W tym wpisie dowiesz się, dlaczego warto wyświetlać informacje debugowania w TypeScript i jak to zrobić.

## Jak to zrobić w TypeScript

Aby wyświetlić informacje debugowania w TypeScript, użyjemy funkcji `console.log()` lub `console.dir()` w odpowiednich miejscach w naszym kodzie. Poniżej znajdują się przykładowe kody z wykorzystaniem tych funkcji oraz oczekiwane wyjście w konsoli.

```TypeScript
let age: number = 25;
console.log(age);
// Output: 25
```
Funkcja `console.log()` zostanie wywołana z wartością `age` jako argument i wyświetli ją w konsoli.

```TypeScript
let person = {
    name: "Jan",
    age: 30
};
console.dir(person);
/*
Output: Object
        age: 30
        name: "Jan"
*/
```
Funkcja `console.dir()` wyświetli wszystkie elementy obiektu `person` w formacie czytelnym dla człowieka.

## Wskazówki do dalszego zgłębiania tematu

Jeśli chcesz lepiej poznać możliwości wyświetlania informacji debugowania w TypeScript, istnieją pewne wskazówki, które mogą Ci pomóc. Przede wszystkim, możesz użyć funkcji `console.assert()` do sprawdzania warunków i wyświetlania błędów w przypadku niezgodności.

```TypeScript
console.assert(age >= 18, "Użytkownik musi mieć co najmniej 18 lat.");
// Output: Assertion failed: Użytkownik musi mieć co najmniej 18 lat.
```

Możesz także korzystać z wbudowanych metod obiektu `Error` do wyświetlania szczegółowych informacji o błędzie. Na przykład:

```TypeScript
if (age < 0) {
    throw new Error("Wiek nie może być ujemny.");
}
// Output: Error: Wiek nie może być ujemny.
```

## Zobacz też

Teraz, gdy już wiesz, dlaczego warto wyświetlać informacje debugowania w TypeScript i jak to zrobić, możesz zgłębić ten temat jeszcze bardziej korzystając z poniższych źródeł:

- [Dokumentacja TypeScript](https://www.typescriptlang.org/docs)
- [Blog TypeScript Heroes](https://typescript.pl/blog/)
- [Kurs TypeScript na Codecademy](https://www.codecademy.com/learn/learn-typescript)