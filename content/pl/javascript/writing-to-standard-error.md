---
title:                "Pisanie do standardowego błędu"
date:                  2024-01-19
html_title:           "Arduino: Pisanie do standardowego błędu"
simple_title:         "Pisanie do standardowego błędu"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
W JavaScript, pisanie do standardowego błędu (stderr) pozwala wyświetlać komunikaty o błędach. Programiści używają tego, by oddzielić zwyczajne dane wyjściowe od informacji o błędach, co ułatwia debugowanie i logowanie.

## How to: (Jak to zrobić:)
```javascript
// Prosty przykład:
console.error('To jest błąd!');

// Przykład z wykorzystaniem obiektu Error:
const err = new Error('Coś poszło nie tak');
console.error(err);
```

Sample output:
```
To jest błąd!
Error: Coś poszło nie tak
```

## Deep Dive (Dogłębna analiza)
Stderr w JavaScript pochodzi z konwencji systemów Unix, gdzie strumień błędów oddzielono od standardowego wyjścia. Alternatywą jest zapisywanie błędów do pliku lub innego medium, ale pisanie do stderr jest standardem w przypadku błędów aplikacji. Implementacja w Node.js i przeglądarkach korzysta z globalnego obiektu `console`, oferując metodę `console.error` do wypisywania na stderr.

## See Also (Zobacz też)
- [Node.js Documentation on Console](https://nodejs.org/api/console.html#console Console.error)
- [MDN Web Docs on Console.error()](https://developer.mozilla.org/en-US/docs/Web/API/console/error)
- [The Console API](https://console.spec.whatwg.org/#loggingapis)
  
Notka: Linki są do angielskich wersji dokumentacji, gdyż mogą zapewniać najświeższe informacje.
