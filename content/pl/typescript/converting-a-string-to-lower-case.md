---
title:    "TypeScript: Konwertowanie ciągu znaków na małe litery."
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego
Często podczas programowania, musimy zmienić wszystkie litery w stringu na małe. Jest to przydatne w wielu sytuacjach, np. porównując dwa stringi czy w trakcie filtrowania listy słów. W tym artykule dowiesz się, jak w prosty sposób wykonać konwersję stringu na małe litery w języku TypeScript.

## Jak to zrobić
Zanim przejdziemy do przykładowego kodu, ważne jest, aby wprowadzić do zmiennej nasz string, którego chcemy dokonać konwersji.

```TypeScript
let str: string = "PROGRAMOWANIE";
```

Aby dokonać konwersji, wykorzystujemy wbudowaną metodę `toLowerCase()` na zmiennej `str`. Następnie przypisujemy zwracaną wartość do nowej zmiennej.

```TypeScript
let strLower: string = str.toLowerCase();

console.log(strLower); // output: programowanie
```

Po wywołaniu metody `toLowerCase()`, wszystkie litery w naszym stringu zostały zmienione na małe. Dodatkowo, warto zauważyć, że zmienna `str` nie została zmieniona, a jedynie zwrócona została wartość zmienionej wersji stringu.

## Deep Dive
W języku TypeScript istnieje również możliwość wykorzystania operatora `|` do określenia rodzajów danych, jakie mogą wystąpić w naszej zmiennej. Dzięki temu, możemy dopasować metodę `toLowerCase()` do konkretnego typu. Na przykład, jeśli nasza zmienna może przyjąć wartość typu `string` lub `null`, możemy napisać:

```TypeScript
let str: string | null = "JĘZYK PROGRAMOWANIA";

let strLower: string = str!.toLowerCase();

console.log(strLower); // output: język programowania
```

W przypadku tutaj występującego operatora `!`, który informuje TypeScript, że nie będzie żadnego błędu w użyciu metody `toLowerCase()`, ponieważ wartość zmiennej `str` jest zawsze typu `string` lub `null`.

## Zobacz również
- [Dokumentacja języka TypeScript - metoda toLowerCase()](https://www.typescriptlang.org/docs/handbook/strings.html#method-tolowercase)
- [Tutorial: Wprowadzenie do języka TypeScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/A_re-introduction_to_JavaScript)
- [15 przydatnych metod string w TypeSript](https://www.javatpoint.com/typescript-string-methods)