---
title:                "Pobieranie aktualnej daty"
date:                  2024-01-20T15:16:56.837979-07:00
html_title:           "Bash: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Pobieranie bieżącej daty to podstawowa umiejętność – pozwala aplikacjom na uświadamianie sobie "teraz". Programiści robią to dla logów, znaczników czasowych i wszelkich funkcji zależnych od czasu.

## How to: (Jak to zrobić:)
```TypeScript
const currentDate: Date = new Date();
console.log(currentDate);
// Wyświetla coś w rodzaju: 2023-01-30T14:25:37.000Z
```
```TypeScript
// Formatowanie dla czytelności:
console.log(currentDate.toLocaleDateString('pl-PL'));
// Wyświetla format daty zgodny z ustawieniami regionalnymi, np. '30.01.2023'
```

## Deep Dive (Dogłębna analiza)
JavaScript (i TypeScript, jako jego nadzbiór) wykorzystuje obiekt `Date` do pracy z datami i czasem. Obiekt ten pochodzi z początków JavaScriptu, kiedy to Brendan Eich tworzył język. Jest kilka alternatyw takich jak biblioteki `date-fns` czy `moment.js`, które oferują więcej opcji i lepszą strefę czasową, ale są też większe i nie zawsze potrzebne. TypeScript dodaje typesafety, ale manipulacja datą leży po stronie standardowych możliwości JavaScriptu.

## See Also (Zobacz także)
- MDN Web Docs na temat Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Dokumentacja TypeScript: https://www.typescriptlang.org/docs/
- 'date-fns' biblioteka: https://date-fns.org/
- 'moment.js' biblioteka: https://momentjs.com/
