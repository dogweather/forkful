---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Zdobycie aktualnej daty oznacza pobranie date od komputera bieżącego użytkownika w danym momencie. Programiści robią to, aby śledzić czas wydarzeń, tworzyć znaczniki daty lub do synchronizacji danych.

## Jak to zrobić:

Oto prosta metoda, aby uzyskać aktualną datę w TypeScript:

```TypeScript
let aktualnaData = new Date();
console.log(aktualnaData);
```

Powinieneś zobaczyć wynik podobny do poniższego, który pokazuje aktualną datę i czas:

```TypeScript
2022-03-04T11:37:29.105Z
```

Możemy również łatwo uzyskać składniki daty, takie jak rok, miesiąc i dzień:

```TypeScript
let rok = aktualnaData.getFullYear();
let miesiac = aktualnaData.getMonth()+1;
let dzien = aktualnaData.getDate();
console.log(`Rok: ${rok}, Miesiąc: ${miesiac}, Dzień: ${dzien}`);
```

Wynik może wyglądać tak:

```TypeScript
Rok: 2022, Miesiąc: 3, Dzień: 4
```

## W głąb tematu

Zakładanie dat jest tak starym konceptem jak samo programowanie. JavaScript, na którym jest oparty TypeScript, zapewnia rodzimą klasę `Date`, którą używamy do manipulacji datą i czasem. TypeScript zachowuje te same metody do obsługi dat, ale dodaje mocne typowanie i inne funkcje.

Alternatywą dla wbudowanego obiektu Date może być biblioteka `moment.js`, która oferuje wiele potężnych funkcji do manipulowania i formatowania dat. 

Oto jak możemy uzyskać aktualną datę za pomocą `moment.js`:

```TypeScript
import * as moment from 'moment';
let aktualnaData = moment();
console.log(aktualnaData.format());
```

Co do szczegółów implementacji, ważne jest pamiętanie, że metoda `getMonth()` zwraca wartość od 0 (styczeń) do 11 (grudzień). Dlatego dodajemy 1 do wyniku, aby uzyskać typowe wartości miesięcy.

## Zobacz też

1. [Dokumentacja TypeScript](https://www.typescriptlang.org/docs/)
2. [Dokumentacja JavaScript Date](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date)
3. [Dokumentacja Moment.js](https://momentjs.com/docs/)