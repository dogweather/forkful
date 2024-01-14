---
title:                "Javascript: Pisanie do standardowego błędu"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego warto umieć pisać do standardowego błędu?

Pisanie do standardowego błędu jest niezbędną umiejętnością dla każdego programisty. Pozwala ono na monitorowanie występujących błędów podczas uruchamiania kodu i szybkie ich naprawianie. Dzięki temu proces tworzenia oprogramowania jest bardziej efektywny i przyspiesza czas dostarczenia działającego produktu.

## Jak to zrobić?

Aby pisać do standardowego błędu w języku Javascript, wystarczy użyć funkcji `console.error()`. Przykładowy kod wyglądałby następująco:

```Javascript
console.error("Błąd: Niedostępny plik");
```

Wywołanie tej funkcji spowoduje wyświetlenie wiadomości o błędzie w konsoli przeglądarki lub środowisku node.js. Można również przekazać więcej argumentów do funkcji, dzięki czemu błąd będzie zawierał szczegółowe informacje na temat jego przyczyny.

## Wnikliwszy przegląd

Pisanie do standardowego błędu jest przydatne nie tylko podczas debugowania kodu, ale także do obsługi wyjątków. Można wykorzystać instrukcję `try...catch` do przechwytywania błędów i wyświetlania ich w konsoli przy użyciu funkcji `console.error()`.

Kolejną zaletą pisania do standardowego błędu jest możliwość przekierowania informacji o błędzie do pliku dziennika (ang. log file). Jest to szczególnie przydatne w przypadku aplikacji internetowych, gdzie dostęp do konsoli przeglądarki jest utrudniony.

## Zobacz także

- [Dokumentacja funkcji console.error() w języku Javascript](https://developer.mozilla.org/pl/docs/Web/API/Console/error)
- [Przewodnik po obsłudze błędów w języku Javascript](https://www.w3schools.com/js/js_errors.asp)
- [Przykładowy kod z wykorzystaniem funkcji console.error()](https://www.geeksforgeeks.org/javascript-console-error-with-examples/)