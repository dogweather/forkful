---
title:                "Javascript: Pobieranie aktualnej daty"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Dlaczego warto poznać bieżącą datę w programowaniu Javascript?

Poznanie bieżącej daty jest ważnym elementem w programowaniu Javascript. Wiele aplikacji i stron internetowych wykorzystuje informacje dotyczące daty, np. do wyświetlania ostatnio dodanych postów czy wydarzeń. Dlatego też warto znać sposoby pozyskiwania bieżącej daty w celu lepszego zrozumienia oraz umiejętnego wykorzystania ich w swoich projektach.

## Jak poznać bieżącą datę w Javascript?

Aby poznać bieżącą datę w programowaniu Javascript, możemy skorzystać z obiektu *Date*. W poniższym przykładzie przedstawione są różne sposoby na pozyskanie bieżącej daty:

```Javascript
// zwraca datę w formacie mm/dd/yyyy
const currentDate = new Date();
console.log(currentDate.toLocaleDateString()); // 11/10/2021

// możemy również określić żądany format daty
console.log(currentDate.toLocaleDateString("pl-PL", { day: "numeric", month: "long", year: "numeric" })); // 10 listopada 2021

// możemy również pozyskać godzinę oraz strefę czasową
console.log(currentDate.toLocaleTimeString("en-US", { hour: "numeric", minute: "numeric", hour12: true })); // 7:43 PM
console.log(currentDate.toLocaleTimeString("en-US", { timeZoneName: "long" })); // Central European Standard Time
```

Możliwości formatowania daty oraz jej czasu w programowaniu Javascript są szerokie i pozwalają na dostosowanie jej do różnych potrzeb.

## Głębszy wykład na temat pozyskiwania bieżącej daty

Obiekt *Date* jest jednym z najważniejszych obiektów w programowaniu Javascript, ponieważ dzięki niemu możemy pracować z datami oraz czasem. Obiekt ten zawiera wiele przydatnych metod, np. *getDate()* czy *getMonth()*, które pozwalają na pozyskanie konkretnych informacji dotyczących daty. Możemy również wykorzystać metody takie jak *setDate()* czy *setFullYear()*, aby zmienić wartość bieżącej daty.

Warto również wspomnieć o strefach czasowych i sposobach ich ustawiania przy pozyskiwaniu daty. W programowaniu Javascript możemy wykorzystać metody takie jak *getTimezoneOffset()* czy *getTimezoneOffset()*, aby dostosować datę do danego miejsca lub formatu. Pozwala to na uniknięcie problemów związanych z różnicami czasowymi w różnych częściach świata.

## Zobacz również:

- [Dokumentacja Date na MDN] (https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/Date)
- [Poradnik pozyskiwania bieżącej daty w Javascript] (https://www.koderman.com/aktualna-data-w-javascript/)
- [Kontrola strefy czasowej w Javascript] (https://medium.com/@torgeadelinjava/javascript-date-object-and-timezone-control-4dfdd9521f22)

Życzę owocnej pracy z pozyskiwaniem bieżącej daty w programowaniu Javascript! ☺️