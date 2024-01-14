---
title:    "Javascript: Pobieranie bieżącej daty"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Dlaczego warto poznać bieżącą datę w programowaniu?

Każda aplikacja musi w jakiś sposób odzwierciedlać aktualną datę. Niezależnie od tego, czy jest to wyświetlanie daty w interfejsie użytkownika, obliczanie czasu trwania wydarzeń lub tworzenie notatek z dnia, programiści potrzebują mechanizmu, który umożliwi im dostęp do bieżącej daty. W tym wpisie na blogu dowiesz się, jak to zrobić za pomocą języka JavaScript.

## Jak uzyskać bieżącą datę w JavaScript

Istnieją różne sposoby na uzyskanie bieżącej daty w języku JavaScript. Jednym z najprostszych jest użycie obiektu `Date()`, który reprezentuje bieżącą datę i czas. Za pomocą metody `getDate()` możemy uzyskać numeryczną wartość dnia miesiąca, a dzięki metodzie `getMonth()` uzyskamy numeryczną wartość miesiąca, gdzie styczeń jest oznaczony jako 0, a grudzień jako 11. Poniższy przykład pokaże, jak to działa:

```javascript
const currentDate = new Date();
console.log(currentDate.getDate()); // 22
console.log(currentDate.getMonth()); // 7 (sierpień)
```

Kolejną przydatną metodą jest `toLocaleDateString()`, która zwraca bieżącą datę w formacie lokalnym. Na przykład, dla urządzeń w Polsce wyświetli datę w formacie "dd.mm.rrrr". Poniższy kod pokazuje, jak to wygląda:

```javascript
const currentDate = new Date();
console.log(currentDate.toLocaleDateString()); // 22.08.2021
```

W celu uzyskania bardziej szczegółowych informacji, jakie metody udostępnia obiekt `Date()`, polecam zapoznać się z dokumentacją języka JavaScript.

## Głębszy zanurknięcie w temat

Podczas programowania często mamy do czynienia z różnymi strefami czasowymi. Dlatego też, warto mieć na uwadze, że obiekt `Date()` nie bierze pod uwagę strefy czasowej w której pracujemy, tylko zwraca datę i czas bieżącego urządzenia. Jeśli potrzebujemy zakodować odpowiednią strefę czasową, możemy użyć metody `toLocaleString()` z dodatkowym parametrem `timeZone`:

```javascript
const options = { timeZone: 'Europe/Warsaw' };
const currentDate = new Date();
console.log(currentDate.toLocaleString('pl-PL', options)); // 22.08.2021, 17:30:00
```

Innym przydatnym zagadnieniem jest możliwość porównywania dat. W tym celu warto przekonwertować datę na wartość liczbującą dzięki metodzie `getTime()`, która zwraca ilość milisekund od 1 stycznia 1970 roku. Dzięki temu możemy łatwo porównywać dwie daty i wykonywać na nich odpowiednie operacje.

# Zobacz również

- [Dokumentacja języka JavaScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference)
- [Metody obiektu Date()](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date)