---
title:                "Javascript: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego pisać w Javascripcie?

Często zdarza się, że w trakcie pisania kodu potrzebujemy informacji o bieżącej dacie. Może to być potrzebne do wyświetlenia daty urodzin na stronie, przeterminowania ważności jakiegoś wydarzenia lub po prostu do monitorowania czasu wykonywania kodu. Dzięki temu artykułowi dowiesz się, jak w łatwy sposób uzyskać aktualną datę przy użyciu języka Javascript.

## Jak to zrobić?

Do uzyskania bieżącej daty w Javascript wykorzystywany jest obiekt Date. Wystarczy stworzyć nową instancję tego obiektu bez podawania żadnych parametrów:

```Javascript
let currentDate = new Date();
console.log(currentDate);
```

W powyższym przykładzie, wywołując metodę `new Date()` otrzymujemy obiekt z aktualną datą i czasem. Następnie wypisujemy ten obiekt przy użyciu funkcji `console.log()`, co zwróci nam datę w formacie `Day Mon Date Year HH:MM:SS Timezone`.

Możemy również precyzyjniej określić datę, podając jako parametryrok, miesiąc i dzień:

```Javascript
let specificDate = new Date(2021, 9, 3);
console.log(specificDate);
```

W powyższym przykładzie określamy datę na 3 października 2021 roku. Pamiętaj, że miesiące w Javascripcie są liczone od zera, więc kolejność parametrów to rok, miesiąc, dzień.

## Wnikliwa analiza

Obiekt Date oferuje wiele innych metod do operowania na dacie. Przykładowo, możemy zwrócić tylko rok, miesiąc lub dzień z aktualnej daty przy pomocy odpowiednio metod `getFullYear()`, `getMonth()` oraz `getDate()`. Możemy również obliczyć różnicę czasu między dwoma datami, używając metody `getTime()` i wykonując proste działania na wartościach zwróconych przez tę metodę.

Warto również zauważyć, że daty w Javascripcie są obiektami, co oznacza, że możemy definiować własne metody i właściwości do pracy z nimi.

## Zobacz także

- [Dokumentacja obiektu Date w Javascript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date)
- [Tutorial: Jak uzyskać bieżącą datę w Javascript](https://www.freecodecamp.org/news/how-to-get-the-current-date-and-time-in-javascript/)
- [Wideo: Praktyczne zastosowania obiektu Date w codziennym kodowaniu](https://www.youtube.com/watch?v=rGCgrPqQH1E)