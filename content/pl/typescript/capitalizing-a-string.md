---
title:                "Zamiana tekstu na wielkie litery"
html_title:           "TypeScript: Zamiana tekstu na wielkie litery"
simple_title:         "Zamiana tekstu na wielkie litery"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co to i dlaczego?

Zwiększanie pierwszej litery stringa to proces zamiany pierwszej litery słowa na dużą. Programiści robią to, aby poprawić czytelność tekstu lub spełnić określone wymagania formatowania.

## Jak to zrobić:

Przykładowego kodu TypeScript, który zwiększa pierwszą literę stringa, można napisać tak:

```TypeScript
function capitalizeFirstLetter(str: string) {
    return str.charAt(0).toUpperCase() + str.slice(1);
}

console.log(capitalizeFirstLetter('tekst'));
```

To wyświetli 'Tekst' w konsoli.

## Szczegółowe omówienie:

Zwiększanie pierwszej litery stringa nie jest konceptem specyficznym dla TypeScript. Jest często stosowane we większości języków programowania, takich jak Python, JavaScript, C#, itp.

Alternatywnie, możemy zastosować bibliotekę Lodash do celów tego zadania. Ta biblioteka zawiera skrypt „capitalize”, który ułatwia tę pracę.

Jednym z ważniejszych aspektów implementacji jest upewnienie się, że jeśli string zawiera więcej niż jedno słowo, tylko pierwsze słowo jest kapitalizowane. Załóżmy, że mówimy o kontekście tekstów headlinerów, gdzie tylko pierwsze słowo w zdaniu powinno być z dużej litery.

## Zobacz także:

Dla dalszego czytania i więcej przykładów, polecam to następujące źródła:
- Typy danych w TypeScript: https://www.typescriptlang.org/docs/handbook/basic-types.html
- Zasoby dotyczące Lodash: https://lodash.com/docs/
- Zasoby dotyczące JavaScript i TypeScript: https://www.typescriptlang.org/docs/handbook/javascript-apis.html