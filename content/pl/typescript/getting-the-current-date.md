---
title:                "Pobieranie aktualnej daty"
html_title:           "TypeScript: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobranie bieżącej daty jest częstym elementem wielu aplikacji, szczególnie tych, które są związane z obsługą czasu i daty, np. kalendarze, dzienniki lub aplikacje do planowania zadań. Warto więc opanować sposób na pobranie bieżącej daty w języku TypeScript, aby móc wykorzystać ją w swoich projektach.

## Jak to zrobić

W języku TypeScript istnieje wiele sposobów na pobranie bieżącej daty. Jednym z najłatwiejszych jest użycie wbudowanej metody JavaScript `Date()`, która zwraca aktualną datę i czas. Przykładowy kod wygląda następująco:

```
let currentDate = new Date();

console.log(currentDate);
```

Powyższy kod wyświetli bieżące dane w formacie `Sat Jun 26 2021 21:27:35 GMT+0200 (Central European Summer Time)`.

Jeśli chcemy wyświetlić datę w bardziej czytelny sposób, możemy skorzystać z metody `toLocaleDateString()`, która zwraca datę w lokalnym formacie, np. w Polsce będzie to `26.06.2021`. Przykładowy kod:

```
let currentDate = new Date();

console.log(currentDate.toLocaleDateString());
```

Możemy również korzystać z pakietów zewnętrznych, takich jak `moment.js`, które oferują jeszcze większą elastyczność i funkcjonalność przy pracy z datami. W takim przypadku musimy najpierw zainstalować pakiet za pomocą menedżera pakietów, a następnie zaimportować go do naszego pliku TypeScript. Przykładowy kod wykorzystujący bibliotekę `moment.js`:

```
import moment from 'moment';

let currentDate = moment();

console.log(currentDate.format('DD.MM.YYYY'));
```

## W czym tkwi głębsza magia?

Pomimo że pobranie bieżącej daty wydaje się być prostym zadaniem, kryje się za tym wiele bardziej złożonych procesów. Na przykład, niektóre systemy operacyjne lub przeglądarki mogą mieć różne strefy czasowe, co może skutkować różnicami w pobieraniu daty. Dodatkowo, identyfikacja bieżącej daty może również wymagać synchronizacji z serwerem czasu lub korzystania z matematycznych operacji, takich jak przeliczanie jednostek czasu. Dlatego warto wypróbować różne metody pobierania daty i dostosować je do własnych potrzeb.

## Zobacz także

- [Dokumentacja Date w języku TypeScript](https://www.typescriptlang.org/docs/handbook/internationalization.html#date-time-intl)
- [Biblioteka moment.js](https://momentjs.com/)
- [Artykuł o przeliczaniu strefy czasowej w TypeScript](https://digitalfortress.tech/tutorial/how-to-convert-a-timezone-in-typescript/)