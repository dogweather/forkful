---
title:                "Ekstrakcja podciągów"
html_title:           "TypeScript: Ekstrakcja podciągów"
simple_title:         "Ekstrakcja podciągów"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

Czym jest ekstrakcja podłańcucha i dlaczego programiści to robią?

Ekstrakcja podłańcucha to proces wyodrębniania części tekstu z dłuższego ciągu znaków. Programiści często wykonują tę czynność w celu manipulacji tekstem lub uzyskania informacji z określonych miejsc w ciągu znaków.

Jak to zrobić:

```TypeScript
// Wyodrębnianie podłańcucha za pomocą indeksu
let tekst = "Hello World!";
console.log(tekst[0]); // Wyświetli "H"

// Wyodrębnianie podłańcucha za pomocą metody .substring()
let zdanie = "To jest przykładowe zdanie.";
console.log(zdanie.substring(8, 17)); // Wyświetli "przykładowe"
```

Pogłębiony wykład:

Ekstrakcja podłańcucha była potrzebna od samego początku programowania, ponieważ umożliwia programiście dostęp do konkretnych części tekstu. W TypeScript istnieje wiele sposobów na wyodrębnianie podłańcucha, takich jak użycie indeksu lub metody .substring(). Alternatywnym sposobem na wykonanie tej czynności jest użycie wyrażeń regularnych. W implementacji ekstrakcji podłańcucha ważne jest również uwzględnienie różnych systemów znaków i obsługa języków o odmiennych strukturach alfabetycznych.

Zobacz również:

Dokumentacja TypeScript do obsługi łańcuchów znaków: https://www.typescriptlang.org/docs/handbook/basic-types.html#string
Przykładowe użycie wyrażeń regularnych do ekstrakcji podłańcucha: https://attacomsian.com/blog/javascript-extract-substring-with-regex