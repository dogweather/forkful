---
title:                "Pisanie do standardowego wyjścia błędów"
html_title:           "TypeScript: Pisanie do standardowego wyjścia błędów"
simple_title:         "Pisanie do standardowego wyjścia błędów"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu jest nieodłączną częścią procesu programowania w TypeScript. Jest to ważny proces, ponieważ pozwala nam na lepsze śledzenie błędów i szybsze rozwiązywanie problemów podczas debugowania.

## Jak to zrobić

Aby napisać do standardowego błędu w TypeScript, należy użyć metody `console.error ()`. Przykładowy kod wyglądałby następująco:

```TypeScript
console.error("To jest przykładowy błąd."); 
```

Po wykonaniu tego kodu otrzymamy następujący wynik w konsoli:

```
To jest przykładowy błąd.
```

Korzystając z tej metody, możemy przekazać dowolny komunikat błędu, który będzie wyświetlony w konsoli, wraz z informacjami o błędzie, na przykład o błędnych zmiennych lub funkcjach.

## Głębszy wgląd

W TypeScript istnieje również inna metoda do pisania do standardowego błędu - `process.stderr.write ()`. Za jej pomocą można wysłać również dane do standardowego błędu. Przykładowy kod wyglądałby następująco:

```TypeScript
process.stderr.write("To są przykładowe dane do błędu.");
```

Wykorzystując tę metodę, możemy przekazać również tablicę dowolnych danych, a nie tylko komunikat tekstowy, co może być przydatne podczas debugowania błędów.

## Zobacz też

- [Dokumentacja TypeScript o standardowym wyjściu i błędach](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html)
- [Informacje o standardowych błędach w JavaScript](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)