---
title:                "Obsługa błędów"
date:                  2024-01-26T00:54:39.654739-07:00
model:                 gpt-4-1106-preview
simple_title:         "Obsługa błędów"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/handling-errors.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obsługa błędów to sposób zarządzania sytuacjami, gdy w kodzie pojawiają się problemy. Jest kluczowa, ponieważ pozwala programom na łagodne awarie i dostarcza użytkownikom klarowne instrukcje, zamiast po prostu zawieść i zakończyć działanie.

## Jak to zrobić:

Oto klasyczny blok `try-catch`:

```javascript
try {
  // Kod, który może spowodować błąd
  let result = potentiallyRiskyOperation();
  console.log('Sukces:', result);
} catch (error) {
  // Co zrobić, gdy zostanie rzucony błąd
  console.error('Ups:', error.message);
}
```

Przykładowe wyjście, gdy nie wystąpi błąd:
```
Sukces: 42
```

A kiedy pojawi się błąd:
```
Ups: Coś poszło nie tak
```

Dla kodu asynchronicznego, gdzie używane są promisy, użyj `try-catch` w funkcji `async`:

```javascript
async function fetchData() {
  try {
    let data = await fetch('https://api.example.com/data');
    console.log('Dane pobrane:', data);
  } catch (error) {
    console.error('Błąd pobierania danych:', error.message);
  }
}

fetchData();
```

## Wgłębienie się

Obsługa błędów w JavaScript przeszła ewolucję. Dawniej (ES3, około 1999 roku), mieliśmy tylko blok `try-catch`. Nie był super elastyczny, ale dawał sobie radę.

ES6 (2015) wprowadziło Promisy i dostarczyło nam `.then()` i `.catch()`, pozwalając na bardziej eleganckie radzenie sobie z błędami asynchronicznymi.

```javascript
fetch('https://api.example.com/data')
  .then(dane => console.log('Dane pobrane:', dane))
  .catch(error => console.error('Błąd pobierania danych:', error.message));
```

Jeśli chodzi o szczegóły implementacji, kiedy błąd jest rzucany, silniki JavaScript tworzą obiekt `Error` z przydatnymi właściwościami takimi jak `message` i `stack`. Możesz także stworzyć własne typy błędów przez rozszerzenie klasy `Error` – przydatne dla bardziej złożonych aplikacji.

Alternatywy? Można ignorować obsługę błędów (zły pomysł), używać funkcji zwrotnych z pierwszym parametrem błędu (witaj, styl Node.js) lub zafascynować się bibliotekami i frameworkami, które oferują własne podejścia.

## Zobacz również

Więcej o obsłudze błędów:

- MDN o try-catch: [MDN try...catch](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Statements/try...catch)
- Asynchroniczne oczekiwanie: [MDN async function](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Statements/async_function)
- Przewodnik po Promisach: [MDN Promises](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Promise)
- Tworzenie i rzucanie własnych błędów: [MDN Error](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Error)
