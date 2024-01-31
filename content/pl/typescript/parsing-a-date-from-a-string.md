---
title:                "Przetwarzanie daty ze łańcucha znaków"
date:                  2024-01-20T15:38:50.753970-07:00
html_title:           "Arduino: Przetwarzanie daty ze łańcucha znaków"
simple_title:         "Przetwarzanie daty ze łańcucha znaków"

category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsowanie daty z ciągu znaków to przekształcenie tekstu na format daty, który rozumie komputer. Robimy to, żeby łatwiej manipulować datami i godzinami - porównywać je, składować w bazie danych, wyświetlać w różnych formatach.

## Jak to zrobić:
```TypeScript
// Zaimportuj funkcję parseISO z date-fns
import { parseISO } from 'date-fns';

// Przykładowy ciąg znaków z datą
const dateString: string = '2023-04-05T14:30:00.000Z';

// Parsowanie ciągu znaków do obiektu Date
const parsedDate: Date = parseISO(dateString);

// Wyświetlenie przeparsowanej daty
console.log(parsedDate);
// Output: 2023-04-05T14:30:00.000Z (data w formacie UTC)
```
Date-fns to biblioteka pomagająca w operacjach na datach. W powyższym przykładzie wykorzystujemy `parseISO` do parsowania daty w formacie ISO 8601.

## Deep Dive
W JavaScript i TypeScript przetwarzanie dat nie zawsze było proste. Wcześniej, trzeba było polegać na wbudowanym obiekcie Date, który bywał problematyczny. Formaty dat były interpretowane różnie w zależności od przeglądarki. Biblioteki takie jak moment.js czy date-fns pojawiły się aby ułatwić i ustandardyzować pracę z datami. Alternatywą dla zewnętrznych bibliotek jest `Date.parse()` z JavaScript, ale często preferowane są biblioteki ze względu na większą elastyczność i lepsze zarządzanie strefami czasowymi. Przy implementacji ważne jest uwzględnienie strefy czasowej oraz formatu daty, który czasami może wymagać specyficznego wzorca.

## Zobacz również
- [date-fns Documentation](https://date-fns.org/v2.28.0/docs/parseISO) - dokumentacja funkcji parseISO z date-fns.
- [MDN Web Docs - Date.parse()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse) - szczegóły na temat Date.parse() w JavaScript.
- [ISO 8601](https://www.iso.org/iso-8601-date-and-time-format.html) - więcej o standardzie ISO 8601 dla formatowania dat i czasu.
