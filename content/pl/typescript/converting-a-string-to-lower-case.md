---
title:    "TypeScript: Konwersja ciągu znaków na małe litery"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Dlaczego
Kazdy programista czasem natyka się na problemy związane z przetwarzaniem tekstu. Jednym z częstych zadań jest konwertowanie tekstu na małe litery. W tym artykule opowiemy Wam, dlaczego warto stosować tę operację i jak można to zrobić w języku TypeScript.

## Jak To Zrobić
Konwertowanie tekstu na małe litery jest bardzo proste w języku TypeScript. Wystarczy użyć metody `toLowerCase()` na obiekcie typu `string` i przypisać go do zmiennej. Poniżej przedstawiamy przykład kodu wraz z oczekiwanym wyjściem:

```TypeScript
let text: string = "HELLO WORLD";
console.log(text.toLowerCase()); // wyjście: hello world
```

Można również zastosować tę metodę do stałych tekstowych, jak również do wyników innych funkcji. Przykład:

```TypeScript
console.log("Bye".toLowerCase()); // wyjście: bye
console.log((3 * 4).toString().toLowerCase()); // wyjście: 12
```

## Deep Dive
Jak działa konwertowanie na małe litery w języku TypeScript? W rzeczywistości metoda `toLowerCase()` jest metodą obiektu `String`, a nie samego typu `string`. Oznacza to, że każda zmienna typu `string` jest automatycznie zamieniana na wartość typu `String`, która posiada wbudowaną metodę `toLowerCase()`.

Co ciekawe, ta metoda jest zdefiniowana w standardzie ECMAScript, więc działa także w innych językach programowania opartych na ECMAScript, takich jak JavaScript czy Java. Metoda `toLowerCase()` jest również często używana wraz z innymi operacjami na tekście, takimi jak `split()` czy `join()`, co czyni ją bardzo przydatną w programowaniu.

## Zobacz Również
Poniżej przedstawiamy dodatkowe artykuły i przykładowe kody, które mogą być pomocne w przetwarzaniu tekstu w języku TypeScript:

- [Metody obiektu `String` w języku TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Przetwarzanie tekstu w języku JavaScript](https://www.w3schools.com/js/js_string_methods.asp)
- [Metody ECMAScript w języku Java](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)

Dziękujemy za lekturę naszego artykułu i do zobaczenia następnym razem!

## Zobacz Również

- [Oficjalna dokumentacja TypeScript](https://www.typescriptlang.org/docs/home.html)
- [Kurs programowania w języku TypeScript](https://www.edx.org/course/programming-typescript)