---
title:                "Wysłanie żądania http"
html_title:           "TypeScript: Wysłanie żądania http"
simple_title:         "Wysłanie żądania http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wysyłanie zapytań HTTP to podstawowa funkcjonalność w programowaniu, polegająca na komunikacji z serwerem w celu pobrania lub przesłania danych. Programiści używają tego mechanizmu, aby otrzymać informacje z zewnętrznych źródeł lub przekazać informacje do innej aplikacji.

## Jak to zrobić:
W TypeScriptie można użyć funkcji ```fetch```, aby wysłać zapytanie HTTP. Przykładowe użycie może wyglądać następująco:
```
fetch('https://example.com/api/users')
  .then(response => response.json())
  .then(data => console.log(data));
```
Wynik zapytania będzie przechowywany w zmiennej `data`, która zawiera obiekt zawierający odpowiedź serwera.

## Deep Dive:
Wysyłanie zapytań HTTP jest nieodłączną częścią aplikacji sieciowych od samego początku istnienia internetu. Obecnie różne języki programowania oferują różne metody do realizacji tego zadania. W TypeScript można również użyć biblioteki Axios, która zapewnia bardziej zaawansowane funkcje takie jak obsługa błędów i interakcja z API z użyciem promise'ów.

## Zobacz także:
- [Dokumentacja TypeScript dla funkcji fetch](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-7.html#introduction-of-the-built-in-data-types-object-assign-object-entries-object-getownpropertydescriptors-object-values-object-is)
- [Oficjalna dokumentacja Axios](https://github.com/axios/axios)