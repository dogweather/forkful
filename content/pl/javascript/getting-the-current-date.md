---
title:                "Otrzymywanie aktualnej daty"
html_title:           "Javascript: Otrzymywanie aktualnej daty"
simple_title:         "Otrzymywanie aktualnej daty"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie aktualnej daty jest jednym z ważnych zadań programistów. Dzięki tej funkcji możemy uzyskać informacje o bieżącym czasie, które są niezbędne do wielu procesów, takich jak obróbka danych, generowanie raportów lub aktualizacja czasu w aplikacjach internetowych.

## Jak to zrobić:

```Javascript
const currentDate = new Date(); 
console.log(currentDate); // Wyświetli bieżącą datę w konsoli
```
W powyższym przykładzie używamy wbudowanego obiektu Date w celu utworzenia nowego obiektu daty o aktualnym czasie. Następnie wyświetlamy ten obiekt w konsoli, aby uzyskać bieżącą datę.

## Głębszy przegląd:

### Kontekst historyczny:

Funkcja pobierania aktualnej daty jest obecna w języku Javascript od samego początku. Pierwsza wersja języka, wydana w 1995 roku, zawierała konstruktor Date, który umożliwiał uzyskanie bieżącej daty. Od tamtego czasu zostały wprowadzone pewne zmiany i ulepszenia, ale funkcjonalność pozostała taka sama.

### Alternatywy:

Ponieważ obiekt Date nie jest jedynym sposobem na uzyskanie aktualnej daty w Javascript, istnieje kilka alternatywnych rozwiązań. Jednym z nich jest biblioteka Moment.js, która oferuje bogate funkcje manipulacji datami. Innym wyborem jest biblioteka date-fns, która jest lżejsza i szybsza od Moment.js.

### Szczegóły implementacji:

Obiekt Date zawiera wiele metod, które umożliwiają manipulację datami i czasami. Na przykład, można dodać lub odjąć kilka dni od aktualnej daty przy użyciu metody setDate(). Dodatkowo, możemy uzyskać poszczególne elementy daty, takie jak dzień, miesiąc czy rok, korzystając z odpowiednich metod, np. getDate().

## Zobacz też:

- [Obiekt Date w dokumentacji Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js - biblioteka do manipulacji datami](https://momentjs.com/)
- [date-fns - biblioteka do manipulacji datami i czasami](https://date-fns.org/)