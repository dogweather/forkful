---
title:                "Javascript: Konwersja daty na ciąg znaków"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie daty na łańcuch znaków jest niezbędnym elementem programowania w JavaScript. Pozwala nam to wyświetlać daty w czytelny sposób dla użytkowników lub przechowywać je w wybranej formie.

## Jak to zrobić

```Javascript
const date = new Date();

// Konwertowanie daty na łańcuch znaków w formacie YYYY-MM-DD
const dateString = date.toISOString().split('T')[0];
console.log(dateString); // wyświetli "2020-04-27"

// Konwertowanie daty na łańcuch znaków w formacie DD.MM.YYYY
const day = date.getDate();
const month = date.getMonth() + 1;
const year = date.getFullYear();

const dateString = `${day < 10 ? '0' : ''}${day}.${month < 10 ? '0' : ''}${month}.${year}`;
console.log(dateString); // wyświetli "27.04.2020"
```

Powyższe przykłady wykorzystują metodę `toISOString()` oraz wbudowane funkcje daty, takie jak `getDate()`, `getMonth()` i `getFullYear()`, aby uzyskać wybrany format daty w postaci łańcucha znaków. Dzięki temu możemy dostosować wyświetlanie daty do naszych potrzeb i wybrać odpowiedni format dla naszego projektu.

## Głębszy wgląd

Konwertowanie daty na łańcuch znaków może wydawać się prostym zadaniem, ale istnieje wiele innych metod i funkcji, które mogą pomóc nam w tym procesie. Na przykład, możemy wykorzystać metodę `toLocaleString()` do uzyskania daty w postaci lokalizowanej dla danego języka i regionu. Istnieją także biblioteki, takie jak moment.js, które udostępniają wiele użytecznych funkcji do pracy z datami.

## Zobacz także
- [Dokumentacja JavaScript - obiekt Date](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date)
- [Poradnik programisty - Konwertowanie daty na tekst w JavaScript](https://www.freecodecamp.org/news/javascript-date-now-converting-guide-how-to-create-date-object-in-javascript-date-to-iso-string-javascript)
- [Moment.js - biblioteka do pracy z datami](https://momentjs.com/)