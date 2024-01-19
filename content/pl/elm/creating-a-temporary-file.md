---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Tworzenie tymczasowego pliku to prowizoryczne utworzenie okresowego magazynu danych. Programiści robią to, aby przechowywać dane, które są potrzebne tylko przez krótki czas i nie są wymagane na stałe.

## Jak to zrobić:

Niestety, język Elm nie obsługuje bezpośrednio tworzenia plików tymczasowych, gdyż jest przeznaczony głównie do tworzenia aplikacji webowych na klienta i nie posiada natywnego API do manipulacji plikami na dysku. W takim wypadku, musimy zastosować rozwiązanie zewnętrzne. Na przykład, korzystając z serwera Node.js po stronie serwera:

```javascript
var tmp = require('tmp');

tmp.file(function _tempFileCreated(err, path, fd, cleanupCallback) {
  if (err) throw err;

  console.log("Ścieżka pliku: ", path);
  console.log("Deskryptor pliku: ", fd);

  cleanupCallback();
});
```

## Wgłębiając się

Tworzenie plików tymczasowych ma swoje korzenie w początkowych latach Unix’a, gdzie tempo pracy dysków twardeych w porównaniu do szybkości RAM była znacznie wolniejsza.

Alternatywnie, inne rozwiązania obejmują korzystanie z Firebase lub innych baz danych NoSQL dla przechowywania tymczasowych danych. Innym rozwiązaniem jest utworzenie własnej usługi do obsługi plików tymczasowych.

Szczegóły implementacji tworzenia plików tymczasowych zależą od wybranych narzędzi i technologii. 

## Zobacz także:
* Strona npm dla biblioteki 'tmp': [npmjs.com/package/tmp](https://www.npmjs.com/package/tmp)
* Baza danych Firebase od Google: [firebase.google.com](https://firebase.google.com/)
* Inne bazy danych NoSQL: [nosql-database.org](https://nosql-database.org/)