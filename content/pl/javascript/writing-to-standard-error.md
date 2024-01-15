---
title:                "Pisanie do standardowego błędu"
html_title:           "Javascript: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie do standardowego błędu może wydawać się nudnym i niepotrzebnym zadaniem w programowaniu, ale faktem jest, że jest to bardzo ważne. Pisanie do standardowego błędu pozwala nam na przechwytywanie i wyświetlanie informacji o błędach, co ułatwia debugowanie naszego kodu i poprawianie ewentualnych problemów.

## Jak to zrobić

Aby pisać do standardowego błędu w Javascript, należy skorzystać z obiektu console, który dostarcza nam metody do wyświetlania różnego rodzaju informacji w konsoli. Jedną z tych metod jest `console.error()`, która służy do wyświetlania błędów.

Przykładowy kod wykorzystujący `console.error()` wyglądałby następująco:

```Javascript
var x = 10;
var y = "abc";

if(typeof x !== "string") {
  console.error("x nie jest ciągiem znaków!");
}

if(typeof y !== "number") {
  console.error("y nie jest liczbą!");
}
```

Po uruchomieniu tego kodu w konsoli otrzymamy następujący output:

```
x nie jest ciągiem znaków!
y nie jest liczbą!
```

W ten sposób możemy wykorzystać pisanie do standardowego błędu do wychwytywania i wyświetlania informacji o błędach w naszym kodzie.

## Deep Dive

W Javascript, standardowy błąd jest reprezentowany przez obiekt `Error`, który zawiera informacje takie jak wiadomość błędu, nazwa błędu, ścieżka i linia, w której wystąpił błąd. Istnieje również wiele typów błędów, takich jak `SyntaxError`, `TypeError` czy `ReferenceError`, które specyfikują rodzaj błędu.

Aby wyświetlić pełną informację o błędzie, możemy użyć funkcji `console.trace()`, która wyświetli nam cały stos wywołań funkcji prowadzących do wystąpienia błędu.

Możemy także przechwytować i obsługiwać błędy za pomocą instrukcji `try...catch`, co pozwala nam na kontrolowane przejście przez błędy i wykonanie odpowiednich akcji w zależności od rodzaju błędu.

## Zobacz również

- [Dokumentacja console w Javascript](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [Artykuł o pisanie do standardowego błędu w Javascript](https://www.sitepoint.com/proper-error-handling-javascript/)