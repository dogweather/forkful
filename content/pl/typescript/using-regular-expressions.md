---
title:                "Wykorzystanie wyrażeń regularnych"
html_title:           "Arduino: Wykorzystanie wyrażeń regularnych"
simple_title:         "Wykorzystanie wyrażeń regularnych"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Regularne wyrażenia (regex) to wzorce, które pomagają w wyszukiwaniu i manipulowaniu tekstami. Programiści używają ich dla szybkiego i elastycznego przetwarzania tekstu, np. dla walidacji danych wejściowych lub przeszukiwania logów.

## How to: (Jak to zrobić?)
```TypeScript
// Sprawdzanie, czy ciąg znaków pasuje do wzorca email
const emailPattern = /\S+@\S+\.\S+/;
const email = "jan.kowalski@example.com";
const isEmailValid = emailPattern.test(email);
console.log(isEmailValid); // Output: true

// Znajdowanie wszystkich wystąpień hashtagów 
const text = "Lubię #typescript i #programowanie";
const hashtagPattern = /#[\w]+/g;
const hashtags = text.match(hashtagPattern);
console.log(hashtags); // Output: ['#typescript', '#programowanie']

// Zamiana ciągu znaków
const wrongText = "Windows jest lepszy niż Linus.";
const correctedText = wrongText.replace("Linus", "Linux");
console.log(correctedText); // Output: Windows jest lepszy niż Linux.
```

## Deep Dive (Głębsze spojrzenie)
Regularne wyrażenia wywodzą się z matematycznej teorii automatów i formalnych języków, a ich początki sięgają lat 50. XX wieku. Alternatywy dla regex to parsery i dedykowane biblioteki do manipulacji stringami, ale regex oferuje wyjątkową mieszankę mocy i elastyczności. W TypeScript implementacja regex jest bezpośrednio połączona z JavaScript, więc wszystkie możliwości i ograniczenia stosowane w JS obowiązują również w TS.

## See Also (Zobacz także)
- MDN Web Docs na temat regularnych wyrażeń: [https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Regular_Expressions)
- RegExr, narzędzie online do nauki i testowania regex: [https://regexr.com/](https://regexr.com/)
- "You Don't Know JS" - seria książek, w tym część o regex: [https://github.com/getify/You-Dont-Know-JS](https://github.com/getify/You-Dont-Know-JS)