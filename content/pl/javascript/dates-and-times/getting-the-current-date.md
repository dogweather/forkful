---
title:                "Pobieranie aktualnej daty"
date:                  2024-02-03T19:10:01.999548-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pobieranie aktualnej daty"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie bieżącej daty w JavaScript jest podstawowym zadaniem, obejmującym pobieranie i ewentualnie manipulowanie dzisiejszą datą i czasem. Programiści wykonują to, aby wyświetlać daty na stronach internetowych, w aplikacjach, śledzić interakcje użytkowników lub obsługiwać dane wrażliwe czasowo.

## Jak to zrobić:
W czystym JavaScript używa się obiektu `Date`, aby pracować z datami i czasem. Oto jak można uzyskać bieżącą datę i czas:

```javascript
const currentDate = new Date();
console.log(currentDate); // Przykładowe wyjście: Pią Kwi 14 2023 12:34:56 GMT+0100 (Czas letni Wielkiej Brytanii)
```

Aby wyświetlić tylko datę w bardziej przyjaznym dla użytkownika formacie, można użyć metod takich jak `toLocaleDateString()`:

```javascript
console.log(currentDate.toLocaleDateString()); // Przykładowe wyjście: 4/14/2023
```

Dla większej kontroli nad formatem, bardzo popularne są biblioteki firm trzecich, takie jak *Moment.js* czy *date-fns*, chociaż warto wiedzieć, że Moment.js jest obecnie uważany za projekt legacy w trybie utrzymania.

Używając *Moment.js*:

```javascript
const moment = require('moment'); // zakładając Node.js lub użycie bundlera modułów
const formattedDate = moment().format('YYYY-MM-DD');
console.log(formattedDate); // Przykładowe wyjście: 2023-04-14
```

Z *date-fns*, które podkreśla modularność pozwalającą na import tylko tego, co potrzebujesz:

```javascript
const { format } = require('date-fns');
const formattedDate = format(new Date(), 'yyyy-MM-dd');
console.log(formattedDate); // Przykładowe wyjście: 2023-04-14
```

Każde podejście oferuje różne poziomy wygody i elastyczności przy pracy z datami w JavaScript, od wbudowanego obiektu `Date` po bardziej zaawansowane możliwości formatowania i manipulowania dostępne przez biblioteki.
