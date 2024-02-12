---
title:                "Korzystanie z wyrażeń regularnych"
date:                  2024-02-01T22:05:10.558506-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z wyrażeń regularnych"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/google-apps-script/using-regular-expressions.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyrażenia regularne (regex) są wzorcami używanymi do dopasowywania kombinacji znaków w ciągach tekstowych. Programiści wykorzystują je do wyszukiwania, edytowania lub manipulowania tekstem i danymi, co czyni je niezastąpionymi w dziedzinie dopasowywania wzorców i analizy danych.

## Jak to zrobić:

Użycie wyrażeń regularnych w Google Apps Script jest proste dzięki składni opartej na JavaScript. Oto jak możesz włączyć regex do swoich skryptów do wspólnych zadań takich jak wyszukiwanie i walidacja danych.

### Wyszukiwanie ciągów znaków

Załóżmy, że chcesz znaleźć w ciągu znaków określony wzorzec, taki jak adres e-mail. Oto prosty przykład:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Znaleziono: " + found[0]);
  } else {
    Logger.log("Nie znaleziono e-maila.");
  }
}

// Przykładowe użycie
findEmailInText("Skontaktuj się z nami pod adresem info@example.com.");
```

### Walidacja danych

Wyrażenia regularne doskonale sprawdzają się w walidacji danych. Poniżej znajduje się funkcja, która weryfikuje, czy wprowadzony ciąg znaków spełnia proste zasady hasła (przynajmniej jedna duża litera, jedna mała litera i minimum 8 znaków).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Przykładowe wyniki
Logger.log(validatePassword("Str0ngPass")); // Wyniki: true
Logger.log(validatePassword("słabe"));       // Wyniki: false
```

## W głąb tematu

Wyrażenia regularne w Google Apps Script są dziedziczone z JavaScriptu, który został po raz pierwszy ustandaryzowany w specyfikacji języka ECMAScript w czerwcu 1997 roku. Mimo że są potężne, czasami mogą prowadzić do mylącego i trudnego do utrzymania kodu, szczególnie gdy są nadużywane lub używane do skomplikowanych zadań dopasowywania wzorców, które mogłyby być rozwiązane efektywniej przez inne metody parsowania.

Na przykład, chociaż można użyć regex do parsowania HTML lub XML w nagłych przypadkach, robić tak jest zazwyczaj odradzane ze względu na zagnieżdżone i skomplikowane struktury tych dokumentów. Zamiast tego, narzędzia specjalnie zaprojektowane do parsowania takich struktur, jak parsery DOM dla HTML, są bardziej niezawodne i czytelne.

Ponadto, programiści Google Apps Script powinni być świadomi potencjalnych problemów z wydajnością podczas używania skomplikowanych wzorców regex w zadaniach manipulacji dużą ilością tekstu, ponieważ przetwarzanie regex może być intensywne dla CPU. W takich przypadkach, podzielenie zadania na prostsze podzadania lub użycie wbudowanych funkcji manipulacji ciągami mogłoby zapewnić lepszą równowagę między wydajnością a utrzymaniem kodu.
