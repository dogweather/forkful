---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Interpolacja łańcuchów to sprytne wstawianie wartości zmiennych bezpośrednio do tekstu w naszych programach. Ten trik pozwala nam uniknąć skomplikowanych operacji łączenia stringów i sprawia, że nasz kod jest bardziej czytelny.

## Jak to zrobić:

Interpolację stringów w TypeScript można wykonać używając znaku backtick (\`). To jest przykład:

```TypeScript
let imie = 'Jan';
let powitanie = `Cześć, ${imie}!`;
console.log(powitanie);  // wyświetla: Cześć, Jan!
```
Tutaj `${imie}` to miejsce interpolacji. TypeScript zamienia to na wartość zmiennej `imie`.

## Głębsze spojrzenie:

Historia interpolacji stringów sięga języka Perl, ale technika ta zyskała na popularności dzięki Ruby. W TypeScript, jest to natywne rozwiązanie, które jest dużo prostsze i czytelniejsze niż klasyczne łączenie stringów używając dodatkowej operatora (+).

Inne rozwiązania, jak na przykład metoda `concat()`, również mogą być używane do łączenia stringów, ale nie są tak intuicyjne jak interpolacja stringów.

Interpolacja stringów jest realizowana poprzez funkcję `String.raw` w TypeScript. `String.raw` jest domyślną metodą dla literałów szablonów i służy do obróbki surowych ciągów znaków z literałów szablonów.

## Zobacz także:

Garść przydatnych źródeł na temat interpolacji stringów:

- Dokumentacja [MDN Web Docs o literałach szablonów](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/template_strings)
- Artykuł na [Stack Overflow na temat interpolacji stringów](https://stackoverflow.com/questions/3304014/how-to-interpolate-variables-in-strings-in-javascript-without-concatenation)
- Dokumentacja [TypeScript o literałach szablonów](https://www.typescriptlang.org/docs/handbook/2/template-literals.html)