---
title:    "TypeScript: Uzyskiwanie bieżącej daty"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego warto pobrać aktualną datę?

Pobieranie aktualnej daty jest niezwykle prostym zadaniem, które może być bardzo przydatne podczas pisania programów w języku TypeScript. Wiele aplikacji wymaga, aby wyświetlały aktualną datę lub czas, na przykład w celu informowania użytkowników o ostatnim zalogowaniu lub daty ostatniej modyfikacji danych. W tym artykule dowiesz się, jak łatwo pobrać aktualną datę w języku TypeScript.

## Jak to zrobić?

Aby pobrać aktualną datę w języku TypeScript, wykorzystaj metodę `Date.now()` lub konstruktor `Date()`. Oba te podejścia zwracają datę w postaci liczby, która reprezentuje liczbę milisekund, które upłynęły od 1 stycznia 1970 roku. Wybór odpowiedniej metody zależy od Twojej potrzeby.

```TypeScript
// Przykład użycia metody Date.now()
const now = Date.now();
console.log(now); // wyświetli liczbę milisekund

// Przykład użycia konstruktora Date()
const date = new Date();
console.log(date); // wyświetli datę i czas aktualnego dnia
```

Możesz również sformatować wyjście w bardziej czytelny sposób za pomocą metod z klasy `Date`, takich jak `getDate()`, `getMonth()` czy `getFullYear()`. Przykład zastosowania w kodzie:

```TypeScript
const date = new Date();
const day = date.getDate();
const month = date.getMonth();
const year = date.getFullYear();

console.log(`${day}.${month + 1}.${year}`); // wyświetli datę w formacie DD.MM.YYYY
```

## Dogłębne zagłębienie się w temat

Pobieranie aktualnej daty może wydawać się banalnym zadaniem, ale istnieje kilka aspektów, które warto zrozumieć dokładniej. Po pierwsze, warto wiedzieć, że `Date.now()` zwraca liczbę milisekund w standardzie czasu uniwersalnego (UTC), a konstruktor `Date()` zwraca datę i czas w lokalnym czasie używanym przez Twój system. Istnieje również możliwość ustawiania lub zmiany daty i czasu za pomocą metod takich jak `setDate()` czy `setHours()`.

Kolejną ważną rzeczą jest fakt, że aktualna data i czas nie są stałe. Na przykład, jeśli zapisałbyś potrzebną datę i czas w kodzie, w kolejnym uruchomieniu aplikacji mogłaby być już nieaktualna. Dlatego ważne jest, aby pobierać aktualną datę w momencie jej wykorzystania.

## Zobacz także

- [Dokumentacja: klasa Date w języku TypeScript](https://developer.mozilla.org/pl/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Stackoverflow - Pobieranie aktualnej daty za pomocą JavaScript](https://stackoverflow.com/questions/1531093/how-do-i-get-the-current-date-in-javascript)