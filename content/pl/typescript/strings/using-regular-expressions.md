---
title:                "Korzystanie z wyrażeń regularnych"
date:                  2024-02-03T19:18:43.028180-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z wyrażeń regularnych"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Wyrażenia regularne, zwane również regex, to potężne narzędzie do dopasowywania wzorców i wyszukiwania w programowaniu. Programiści używają wyrażeń regularnych do zadań takich jak walidacja danych wejściowych użytkownika, przeszukiwanie tekstu czy manipulowanie ciągami, ponieważ jest to efektywne i wszechstronne.

## Jak to zrobić:

Przejdźmy do TypeScript i zobaczmy, jak używać regex do typowych zadań.

```TypeScript
// Zdefiniuj wzorzec regex dla adresu e-mail
const emailPattern = /\S+@\S+\.\S+/;

// Test, czy ciąg pasuje do wzorca e-mail
const email = "user@example.com";
console.log(emailPattern.test(email)); // Wynik: true

// Znajdź i zamień cyfry w ciągu
const replaceDigits = "Item 25 kosztuje $30".replace(/\d+/g, '#');
console.log(replaceDigits); // Wynik: "Item # kosztuje $#"

// Ekstrahowanie określonych części z ciągu przy użyciu grup przechwytujących
const data = "Kwiecień 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, month, day, year] = datePattern.exec(data) || [];
console.log(month, day, year); // Wynik: "Kwiecień" "10" "2021"
```

## Dogłębnie

W latach 50. matematyk Stephen Kleene opisał wyrażenia regularne jako model do reprezentowania języków regularnych, które później stały się niezbędnym elementem informatyki. Idąc do przodu, regex stał się wszechobecny w programowaniu w kontekście pracy z tekstem.

Chociaż regex jest scyzorykiem szwajcarskim dla operacji na ciągach, nie jest pozbawiony alternatyw. W zależności od złożoności zadania, czasami metody ciągów takie jak `includes()`, `startsWith()`, `endsWith()` lub nawet parsowanie przy użyciu biblioteki mogą być lepsze. Na przykład parsowanie złożonego ciągu JSON za pomocą regex może być koszmarem - użyj zamiast tego parsera JSON.

Jeśli chodzi o implementację, regex w JavaScript i TypeScript opiera się na specyfikacji języka ECMAScript. Pod spodem, silniki używają automatów stanów do efektywnego dopasowywania wzorców. Warto zauważyć, że operacje regex mogą być kosztowne pod względem wydajności, szczególnie przy źle napisanych wzorcach - uważaj na "katastrofalne cofanie się".

## Zobacz także

- MDN Web Docs o Wyrażeniach Regularnych: [MDN Regular Expressions](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101: Narzędzie do testowania i debugowania wzorców regex [Regex101](https://regex101.com/)
- Książka "Opanowanie wyrażeń regularnych" dla dogłębnego zrozumienia: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
