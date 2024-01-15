---
title:                "Konwertowanie daty na ciąg znaków"
html_title:           "Javascript: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest niezbędnym elementem w wielu projektach JavaScript. Pozwala nam wygodnie wyświetlać daty w czytelnej formie dla użytkowników aplikacji oraz przetwarzać je w dalszych operacjach.

## Jak to zrobić

Konwersja daty na ciąg znaków w JavaScript jest prosta i wymaga wykorzystania wbudowanych metod.getFullYear(), .getMonth(), .getDate(), .getHours(), .getMinutes(), .getSeconds(), .getMilliseconds() oraz .toISOString().

```Javascript
let today = new Date();
let dateToString = `${today.getFullYear()}-${today.getMonth() + 1}-${today.getDate()} ${today.getHours()}:${today.getMinutes()}:${today.getSeconds()}`;
console.log(dateToString); // Output: 2021-09-17 14:30:26
```

W powyższym przykładzie użyliśmy metody .getFullYear() do pobrania aktualnego roku, .getMonth() do pobrania aktualnego miesiąca + 1 (ponieważ metoda .getMonth() liczy miesiące od 0 do 11), .getDate() do pobrania aktualnego dnia miesiąca, .getHours(), .getMinutes(), .getSeconds() do pobrania aktualnej godziny, minuty i sekundy oraz metody .toISOString() do zamiany naszego obiektu daty na standardową reprezentację daty i czasu w formacie ISO.

## Głębszy wgląd

W JavaScript istnieją również inne sposoby na formatowanie daty i czasu do ciągu znaków, takie jak metoda .toLocaleString(), która pozwala na wybranie przez nas pożądanej strefy czasowej oraz formatu daty i czasu. W przypadku bardziej zaawansowanych operacji, istnieje także biblioteka Moment.js, która oferuje wiele dodatkowych funkcji do manipulowania datami i czasem.

## Zobacz także

- [Date Object w MDN](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Formatowanie daty i czasu w JavaScript](https://www.digitalocean.com/community/tutorials/how-to-format-dates-in-javascript)