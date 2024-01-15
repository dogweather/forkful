---
title:                "Pobieranie bieżącej daty"
html_title:           "Javascript: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Dlaczego

## Dlaczego warto poznać aktualną datę w JavaScript?

Jeśli tworzysz aplikację lub stronę internetową, która wykorzystuje daty, być może będziesz potrzebować aktualnej daty jako części funkcjonalności. Na przykład, możliwe że chcesz wyświetlać aktualną datę na swojej stronie lub tworzyć wydarzenia w kalendarzu. Dlatego jest ważne znać sposoby na pobranie aktualnej daty w JavaScript.

## Jak to zrobić?

Pobranie aktualnej daty w JavaScript jest bardzo proste. Wystarczy użyć wbudowanego obiektu `Date` i wywołać jego metodę `getDate()` oraz `getMonth()`. Przykładowy kod wygląda następująco:

```javascript
let now = new Date();
let day = now.getDate();
let month = now.getMonth();
```

W powyższym przykładzie, tworzymy nowy obiekt `Date` i przypisujemy go do zmiennej `now`. Następnie korzystamy z jego metod `getDate()` i `getMonth()` aby pobrać odpowiednie wartości (dzień miesiąca i miesiąc). Wywołanie tych metod zwróci liczby, które mogą być użyte do wyświetlenia lub przetworzenia daty dalej.

## Dogłębna analiza

Można również pobrać inne informacje z obiektu `Date`, takie jak rok, godzina, minuta itp. poprzez użycie odpowiednich metod (np. `getFullYear()` lub `getHours()`). Można także manipulować datami, wykorzystując metody takie jak `setDate()` lub `setMonth()`.

Warto również pamiętać, że obiekt `Date` jest dynamiczny, co oznacza, że wywołując jego metody zwróci aktualną datę dla aktualnego dnia i czasu. Aby uzyskać szczegółowe informacje o tym obiekcie oraz jego metodach, warto zajrzeć do dokumentacji na stronie MDN:

https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Data

# Zobacz też

- https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Data
- https://www.w3schools.com/js/js_dates.asp
- https://www.geeksforgeeks.org/javascript-date-methods/