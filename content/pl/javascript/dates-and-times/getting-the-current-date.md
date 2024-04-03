---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:01.999548-07:00
description: "Pobieranie bie\u017C\u0105cej daty w JavaScript jest podstawowym zadaniem,\
  \ obejmuj\u0105cym pobieranie i ewentualnie manipulowanie dzisiejsz\u0105 dat\u0105\
  \ i czasem. Programi\u015Bci\u2026"
lastmod: '2024-03-13T22:44:35.806955-06:00'
model: gpt-4-0125-preview
summary: "Pobieranie bie\u017C\u0105cej daty w JavaScript jest podstawowym zadaniem,\
  \ obejmuj\u0105cym pobieranie i ewentualnie manipulowanie dzisiejsz\u0105 dat\u0105\
  \ i czasem."
title: Pobieranie aktualnej daty
weight: 29
---

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
