---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:43.028180-07:00
description: "Jak to zrobi\u0107: Przejd\u017Amy do TypeScript i zobaczmy, jak u\u017C\
  ywa\u0107 regex do typowych zada\u0144."
lastmod: '2024-03-13T22:44:35.128404-06:00'
model: gpt-4-0125-preview
summary: "Przejd\u017Amy do TypeScript i zobaczmy, jak u\u017Cywa\u0107 regex do typowych\
  \ zada\u0144."
title: "Korzystanie z wyra\u017Ce\u0144 regularnych"
weight: 11
---

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
